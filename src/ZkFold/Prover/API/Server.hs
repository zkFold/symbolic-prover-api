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
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Pool
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Time (diffUTCTime, secondsToNominalDiffTime)
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
import System.Cron.Schedule
import ZkFold.Prover.API.Database (deleteOldProofs, initDatabase)
import ZkFold.Prover.API.Executor
import ZkFold.Prover.API.Handler (api, apiServer)
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
    let sleepTime = fromEnum (nominalDiffTimeToSeconds (kpExpires old `diffUTCTime` time)) `div` (10 ^ (6 :: Int))
    threadDelay sleepTime
    newest <- randomKeyPair period
    atomically $ writeTVar (ctxServerKeys ctx) [new, newest]

oldProofDeleter :: Ctx nip -> Int -> IO ()
oldProofDeleter ctx maxDays = do
    withResource (ctxConnectionPool ctx) $ \conn -> do
        deleteOldProofs conn maxDays

runServer ::
    forall i o.
    ( ProveAlgorithm i o
    , MimeUnrender JSON i
    ) =>
    ServerConfig -> IO ()
runServer ServerConfig{..} = do
    putStrLn "Started with config:"
    LBS.putStrLn $ encodePretty ServerConfig{..}

    let keyLifetime = secondsToNominalDiffTime $ toEnum $ keysLifetime * (10 ^ (12 :: Int))
    oldKey <- randomKeyPair (keyLifetime / 2)
    newKey <- randomKeyPair keyLifetime
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
                }

    _oldProofsDeleterThreadId <- execSchedule $ do
        addJob (oldProofDeleter ctx proofLifetime) "0 0 * * *"
    _keyUpdaterThreadId <- forkIO $ keyUpdater ctx keyLifetime

    replicateM_ nWorkers $ forkIO (proofExecutor @i @o ctx)

    run serverPort $
        logStdout $
            corsMiddleware $
                serve (api @i @o) (apiServer @i @o ctx)