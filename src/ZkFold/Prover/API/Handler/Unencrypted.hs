{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Prover.API.Handler.Unencrypted where

import Control.Concurrent.STM (atomically, writeTQueue)
import Control.Lens ((?~))
import Control.Lens.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy
import Data.Data
import Data.Pool
import Data.Swagger (HasDescription (description), Swagger, applyTagsFor)
import Data.UUID.V4 (nextRandom)
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import ZkFold.Prover.API.Database
import ZkFold.Prover.API.Handler.Delegation
import ZkFold.Prover.API.Handler.General (MainAPI, ProofStatusEndpoint, ProveJSON, StatsEndpoint, V0, baseOpenApi, handleProofStatus, handleStats)
import ZkFold.Prover.API.Orphans ()
import ZkFold.Prover.API.Robots (handleRobots)
import ZkFold.Prover.API.Types
import ZkFold.Prover.API.Types.Encryption ()
import ZkFold.Prover.API.Types.ProveAlgorithm (ProveAlgorithm)
import Prelude hiding (id)

type ProveUnencryptedEndpoint i =
    Summary "Submit unencrypted data for proving."
        :> "prove"
        :> ReqBody '[JSON] i
        :> Post '[JSON] ProofId

type ProverUnencryptedEndpoint i o =
    ProofStatusEndpoint o
        :<|> ProveUnencryptedEndpoint i
        :<|> StatsEndpoint

openApi :: forall i o. (ProveAlgorithm i o) => Swagger
openApi =
    baseOpenApi (toSwagger proxy)
        & applyTagsFor
            (subOperations proxy proxy)
            ["ZK prover endpoints" & description ?~ "Submit a proof and get proof status."]
  where
    proxy = Proxy :: Proxy (V0 :> ProverUnencryptedEndpoint i o)

handleProve :: forall i. (ToJSON i) => Ctx i -> i -> Handler ProofId
handleProve ctx@Ctx{..} w =
    handleProveWithDelegationStrategy ctx w (delegationStrategy ctxDelegationServers w)

handleProveWithDelegationStrategy :: forall i. (ToJSON i) => Ctx i -> i -> Maybe String -> Handler ProofId
handleProveWithDelegationStrategy Ctx{..} w Nothing =
    liftIO $ withResource ctxConnectionPool $ \conn -> do
        uuid <- nextRandom
        addNewProveQuery conn uuid Nothing
        atomically $ writeTQueue ctxProofQueue (uuid, UnencryptedWD w)
        pure $ ProofId uuid
handleProveWithDelegationStrategy ctx@Ctx{..} w (Just url) = do
    eUuid <- liftIO $ sendPostRequest (url <> "/v0/prove") (toStrict $ encode w) customHeaders
    case eUuid of
        Left err -> do
            liftIO $ putStrLn $ "LOG: Error \'" <> show err <> "\' in delegating prove"
            -- TODO: Retry with other server
            handleProveWithDelegationStrategy ctx w Nothing
        Right uuidBytes -> do
            case decode (fromStrict uuidBytes) of
                Nothing -> do
                    liftIO $ putStrLn "LOG: delegation server didn't return correct uuid"
                    handleProveWithDelegationStrategy ctx w Nothing
                Just (ProofId uuid) -> do
                    liftIO $ withResource ctxConnectionPool $ \conn -> do
                        addNewProveQuery conn uuid (Just url)
                        pure $ ProofId uuid

handleProverApi :: forall i o. (ProveJSON i o) => Ctx i -> Servant.Server (V0 :> ProverUnencryptedEndpoint i o)
handleProverApi ctx =
    handleProofStatus ctx
        :<|> handleProve ctx
        :<|> handleStats ctx

mainApi :: forall i o. Proxy (MainAPI (V0 :> ProverUnencryptedEndpoint i o))
mainApi = Proxy

mainServer :: forall i o. (ProveAlgorithm i o, ProveJSON i o) => Ctx i -> Servant.Server (MainAPI (V0 :> ProverUnencryptedEndpoint i o))
mainServer ctx =
    handleProverApi @i @o ctx
        :<|> swaggerSchemaUIServerT (openApi @i @o)
        :<|> handleRobots
