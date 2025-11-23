{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Prover.API.Handler.Encrypted where

import Control.Concurrent.STM (atomically, writeTQueue)
import Control.Lens ((?~))
import Control.Lens.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (toStrict)
import Data.ByteString.Lazy (fromStrict)
import Data.Data
import Data.Pool
import Data.Swagger hiding (Header, Response, headers, url)
import Data.UUID.V4 (nextRandom)
import Servant hiding (Header)
import Servant.Swagger (HasSwagger (toSwagger), subOperations)
import Servant.Swagger.UI
import ZkFold.Prover.API.Database
import ZkFold.Prover.API.Encryption
import ZkFold.Prover.API.Handler.Delegation
import ZkFold.Prover.API.Handler.General (MainAPI, ProofStatusEndpoint, StatsEndpoint, V0, baseOpenApi, handleProofStatus, handleStats)
import ZkFold.Prover.API.Orphans ()
import ZkFold.Prover.API.Robots (handleRobots)
import ZkFold.Prover.API.Types
import ZkFold.Prover.API.Types.Encryption ()
import ZkFold.Prover.API.Types.ProveAlgorithm (ProveAlgorithm)
import Prelude hiding (id)

type KeysEndpoint =
    Summary "Get server public keys."
        :> "keys"
        :> Get '[JSON] [PublicKeyBundle]

type ProveEncryptedEndpoint =
    Summary "Submit data for proving."
        :> "prove"
        :> ReqBody '[JSON] ZKProveRequest
        :> Post '[JSON] ProofId

type ProverEncryptedEndpoints i o =
    ProofStatusEndpoint o
        :<|> KeysEndpoint
        :<|> ProveEncryptedEndpoint
        :<|> StatsEndpoint

openApi :: forall i o. (ProveAlgorithm i o) => Swagger
openApi =
    baseOpenApi (toSwagger proxy)
        & applyTagsFor
            (subOperations proxy proxy)
            ["ZK prover endpoints" & description ?~ "Get server public keys, submit a proof and get proof status."]
  where
    proxy = Proxy :: Proxy (V0 :> ProverEncryptedEndpoints i o)

handleGetKeys :: forall i. Ctx i -> Handler [PublicKeyBundle]
handleGetKeys Ctx{..} = getPublicKeys ctxServerKeys

handleProve :: forall i. (ToJSON i, FromJSON i) => Ctx i -> ZKProveRequest -> Handler ProofId
handleProve ctx@Ctx{..} zkpr =
    handleProveWithDelegationStrategy ctx zkpr (delegationStrategy ctxDelegationServers zkpr)

handleProveWithDelegationStrategy :: forall i. (ToJSON i, FromJSON i) => Ctx i -> ZKProveRequest -> Maybe String -> Handler ProofId
handleProveWithDelegationStrategy Ctx{..} zkpr Nothing =
    liftIO $ withResource ctxConnectionPool $ \conn -> do
        uuid <- nextRandom
        addNewProveQuery conn uuid Nothing
        atomically $ writeTQueue ctxProofQueue (uuid, EncryptedWD zkpr)
        pure $ ProofId uuid
handleProveWithDelegationStrategy ctx@Ctx{..} zkpr (Just url) = do
    eDecryptedInput <- liftIO $ runHandler $ decryptInput ctxServerKeys zkpr :: Handler (Either ServerError i)
    case eDecryptedInput of
        Left err ->
            liftIO $ withResource ctxConnectionPool $ \conn -> do
                uuid <- nextRandom
                putStrLn $ "LOG: Error \'" <> show err <> "\' in decrypting input with id " <> show uuid
                addNewProveQuery conn uuid Nothing
                markAsFailed conn uuid
                pure $ ProofId uuid
        Right decryptedInput -> do
            eUuid <- liftIO $ sendPostRequest (url <> "/v0/prove") (toStrict $ encode decryptedInput) customHeaders
            case eUuid of
                Left err -> do
                    liftIO $ putStrLn $ "LOG: Error \'" <> show err <> "\' in delegating prove request"
                    -- TODO: Retry with other server
                    handleProveWithDelegationStrategy ctx zkpr Nothing
                Right uuidBytes -> do
                    ProofId uuid <- case decode (fromStrict uuidBytes) of
                        Nothing -> do
                            liftIO $ putStrLn "LOG: delegation server didn't return correct uuid"
                            handleProveWithDelegationStrategy ctx zkpr Nothing
                        Just x -> pure x
                    liftIO $ withResource ctxConnectionPool $ \conn -> do
                        addNewProveQuery conn uuid (Just url)
                        pure $ ProofId uuid

handleProverApi :: forall i o. (ToJSON o, FromJSON o, ToJSON i, FromJSON i) => Ctx i -> Servant.Server (V0 :> ProverEncryptedEndpoints i o)
handleProverApi ctx =
    handleProofStatus ctx
        :<|> handleGetKeys ctx
        :<|> handleProve ctx
        :<|> handleStats ctx

mainApi :: forall i o. Proxy (MainAPI (V0 :> ProverEncryptedEndpoints i o))
mainApi = Proxy

mainServer :: forall i o. (ProveAlgorithm i o, ToJSON i) => Ctx i -> Servant.Server (MainAPI (V0 :> ProverEncryptedEndpoints i o))
mainServer ctx =
    handleProverApi @i @o ctx
        :<|> swaggerSchemaUIServerT (openApi @i @o)
        :<|> handleRobots
