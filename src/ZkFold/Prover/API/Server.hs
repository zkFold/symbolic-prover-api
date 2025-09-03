module ZkFold.Prover.API.Server (
  ServerConfig (..),
  runServer,
) where

import Control.Concurrent
import Control.Concurrent.STM (newTQueue, newTQueueIO, newTVarIO)
import Control.Concurrent.STM.TChan (newTChanIO)
import Control.Monad (replicateM_, void)
import Data.Map.Strict qualified as M
import Data.Pool
import Database.PostgreSQL.Simple
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, corsRequestHeaders)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant
import ZkFold.Prover.API.Database
import ZkFold.Prover.API.Handler
import ZkFold.Prover.API.Types

data ServerConfig = ServerConfig
  { port ∷ Int
  , dbHost ∷ String
  , dbName ∷ String
  , dbUser ∷ String
  , dbPassword ∷ String
  , nWorkers ∷ Int
  }
  deriving (Eq, Show)

-- Allow all origins and common methods/headers
simpleCorsResourcePolicy ∷ CorsResourcePolicy
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

corsMiddleware ∷ Middleware
corsMiddleware = cors (const $ Just simpleCorsResourcePolicy)

runServer ∷ ServerConfig → IO ()
runServer ServerConfig {..} = do
  proofsDb ← newTVarIO M.empty
  key ← randomKeyPair
  keysVar ← newTVarIO [key]
  queue ← newTQueueIO
  let connectInfo =
        defaultConnectInfo
          { connectHost = dbHost
          , connectDatabase = dbName
          , connectUser = dbUser
          , connectPassword = dbPassword
          }
  conn ← connect connectInfo
  pool ← newPool $ defaultPoolConfig (connect connectInfo) close 60 20

  void $ execute_ conn createQueryStatusType
  void $ execute_ conn createQueryTable
  void $ execute_ conn createTriggerFunction
  void $ execute_ conn createTrigger

  taskManagerTaskId ← forkIO (taskManager pool queue)

  let
    ctx =
      Ctx
        { ctxProofsDatabase = proofsDb
        , ctxConnectionPool = pool
        , ctxServerKeys = keysVar
        , ctxProofQueue = queue
        }
  replicateM_ nWorkers $ forkIO (proofExecutor ctx)
  run port $ logStdout $ corsMiddleware $ serve mainApi $ mainServer ctx
