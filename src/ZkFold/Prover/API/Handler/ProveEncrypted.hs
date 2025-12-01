module ZkFold.Prover.API.Handler.ProveEncrypted where

import Control.Concurrent.STM (atomically, writeTQueue)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Pool (withResource)
import Data.UUID.V4 (nextRandom)
import Servant
import ZkFold.Prover.API.Database (addNewProveQuery, markAsFailed)
import ZkFold.Prover.API.Encryption (decryptInput)
import ZkFold.Prover.API.Handler.Delegation (customHeaders, delegationStrategy, sendPostRequest)
import ZkFold.Prover.API.Orphans ()
import ZkFold.Prover.API.Types (ProofId (..))
import ZkFold.Prover.API.Types.Ctx (Ctx (..), WitnessData (..))
import ZkFold.Prover.API.Types.Prove (ZKProveRequest)

-- | Type for the encrypted prove endpoint.
type ProveEncryptedEndpoint =
    Summary "Submit encrypted data for proving."
        :> "prove-encrypted"
        :> ReqBody '[JSON] ZKProveRequest
        :> Post '[JSON] ProofId

proveEncryptedServer :: forall i. (ToJSON i, FromJSON i) => Ctx i -> Server ProveEncryptedEndpoint
proveEncryptedServer Ctx{..} = handler
  where
    handler :: ZKProveRequest -> Handler ProofId
    handler zkRequest = do
        strategy <- delegationStrategy ctxDelegationServers zkRequest
        handleProveWithDelegationStrategy zkRequest strategy

    handleProveWithDelegationStrategy :: ZKProveRequest -> Maybe String -> Handler ProofId
    handleProveWithDelegationStrategy zkRequest Nothing =
        liftIO $ withResource ctxConnectionPool $ \conn -> do
            uuid <- nextRandom
            addNewProveQuery conn uuid Nothing
            atomically $ writeTQueue ctxProofQueue (uuid, EncryptedWD zkRequest)
            pure $ ProofId uuid
    handleProveWithDelegationStrategy zkRequest (Just url) = do
        eDecryptedInput <- liftIO $ runHandler $ decryptInput ctxServerKeys zkRequest :: Handler (Either ServerError i)
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
                        handleProveWithDelegationStrategy zkRequest Nothing
                    Right uuidBytes -> do
                        ProofId uuid <- case decode (fromStrict uuidBytes) of
                            Nothing -> do
                                liftIO $ putStrLn "LOG: delegation server didn't return correct uuid"
                                handleProveWithDelegationStrategy zkRequest Nothing
                            Just x -> pure x
                        liftIO $ withResource ctxConnectionPool $ \conn -> do
                            addNewProveQuery conn uuid (Just url)
                            pure $ ProofId uuid
