{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Prover.API.Server (
    ServerConfig (..),
    runServer,
) where

import Control.Concurrent
import Control.Concurrent.STM (newTQueueIO, newTVarIO)
import Control.Monad (replicateM_)
import Data.Aeson
import Data.Binary
import Data.Data
import Data.OpenApi (ToSchema)
import Data.Pool
import Database.PostgreSQL.Simple
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, corsRequestHeaders)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant
import ZkFold.Protocol.NonInteractiveProof
import ZkFold.Prover.API.Executor
import ZkFold.Prover.API.Handler
import ZkFold.Prover.API.Types

data ServerConfig = ServerConfig
    { serverPort :: Int
    , dbPort :: Word16
    , dbHost :: String
    , dbName :: String
    , dbUser :: String
    , dbPassword :: String
    , nWorkers :: Int
    , contractId :: Int
    }
    deriving (Eq, Show)

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

runServer ::
    forall nip p w.
    ( NonInteractiveProof nip
    , w ~ Witness nip
    , p ~ Proof nip
    , FromJSON w
    , ToJSON p
    , FromJSON p
    , ToSchema w
    , ToSchema p
    , Typeable nip
    , MimeUnrender JSON w
    ) =>
    ServerConfig -> SetupProve nip -> IO ()
runServer ServerConfig{..} sp = do
    key <- randomKeyPair
    keysVar <- newTVarIO [key]
    queue <- newTQueueIO
    let connectInfo =
            defaultConnectInfo
                { connectHost = dbHost
                , connectDatabase = dbName
                , connectUser = dbUser
                , connectPassword = dbPassword
                , connectPort = dbPort
                }
    pool <- newPool $ defaultPoolConfig (connect connectInfo) close 60 20

    let
        ctx =
            Ctx
                { ctxConnectionPool = pool
                , ctxServerKeys = keysVar
                , ctxProofQueue = queue
                , ctxContractId = contractId
                }
    replicateM_ nWorkers $ forkIO (proofExecutor @nip @p @w ctx sp)
    run serverPort $ logStdout $ corsMiddleware $ serve (mainApi @nip) $ mainServer @nip ctx
