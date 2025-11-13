{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Prover.API.Server (
    ServerConfig (..),
    runServer,
) where

import Control.Concurrent
import Control.Concurrent.STM (newTQueueIO, newTVarIO, writeTVar)
import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Monad (forever, replicateM_)
import Control.Monad.STM (atomically)
import Data.Pool
import Data.Time (diffUTCTime, nominalDay)
import Data.Time.Clock (
    NominalDiffTime,
    getCurrentTime,
    nominalDiffTimeToSeconds,
 )
import Database.SQLite.Simple
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, corsRequestHeaders)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant
import ZkFold.Prover.API.Database (initDatabase)
import ZkFold.Prover.API.Executor
import ZkFold.Prover.API.Handler.Encrypted qualified as Encrypted
import ZkFold.Prover.API.Handler.Unencrypted qualified as Unencrypted
import ZkFold.Prover.API.Types
import ZkFold.Prover.API.Types.Config
import ZkFold.Prover.API.Types.ProveAlgorithm (ProveAlgorithm)

-- Allow all origins and common methods/headers
simpleCorsResourcePolicy :: CorsResourcePolicy
simpleCorsResourcePolicy =
    CorsResourcePolicy
        { corsOrigins = Nothing -- Nothing means allow all origins
        , corsMethods = ["GET", "POST", "OPTIONS"]
        , corsRequestHeaders = ["Content-Type"]
        , corsExposedHeaders = Nothing
        , corsMaxAge = Just 3600
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
        }

corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just simpleCorsResourcePolicy)

keyUpdater :: Ctx nip -> NominalDiffTime -> IO ()
keyUpdater ctx period = forever $ do
    [old, new] <- readTVarIO $ ctxServerKeys ctx
    time <- getCurrentTime
    let sleepTime = fromEnum (nominalDiffTimeToSeconds (kpExpires old `diffUTCTime` time)) `div` 1000000
    threadDelay sleepTime
    newest <- randomKeyPair period
    atomically $ writeTVar (ctxServerKeys ctx) [new, newest]

runServer ::
    forall i o.
    ( ProveAlgorithm i o
    , MimeUnrender JSON i
    ) =>
    ServerConfig -> IO ()
runServer ServerConfig{..} = do
    let period = nominalDay
    oldKey <- randomKeyPair (period / 2)
    newKey <- randomKeyPair period
    keysVar <- newTVarIO [oldKey, newKey]
    queue <- newTQueueIO

    pool <- newPool $ defaultPoolConfig (open dbFile) close 60 20
    withResource pool initDatabase

    let
        ctx =
            Ctx
                { ctxConnectionPool = pool
                , ctxServerKeys = keysVar
                , ctxProofQueue = queue
                , ctxContractId = contractId
                , ctxEncryptionMode = encryptionMode
                }

    _keyUpdaterThreadId <- forkIO $ keyUpdater ctx period

    replicateM_ nWorkers $ forkIO (proofExecutor @i @o ctx)

    run serverPort $
        logStdout $
            corsMiddleware $
                case ctxEncryptionMode ctx of
                    EncryptedMode -> serve (Encrypted.mainApi @i @o) $ Encrypted.mainServer @i @o ctx
                    UnencryptedMode -> serve (Unencrypted.mainApi @i @o) $ Unencrypted.mainServer @i @o ctx