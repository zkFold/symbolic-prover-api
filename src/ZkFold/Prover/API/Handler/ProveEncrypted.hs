module ZkFold.Prover.API.Handler.ProveEncrypted where

import Control.Concurrent.STM (atomically, writeTQueue)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Pool (withResource)
import Servant
import ZkFold.Prover.API.Database (addNewProveQuery, isProofExist, markAsFailed)
import ZkFold.Prover.API.Encryption (decryptInput)
import ZkFold.Prover.API.Handler.Delegation (customHeaders, delegationStrategy, sendPostRequest)
import ZkFold.Prover.API.Orphans ()
import ZkFold.Prover.API.Types (ProofId (..))
import ZkFold.Prover.API.Types.Ctx (Ctx (..), WitnessData (..))
import ZkFold.Prover.API.Types.Prove (
    ZKProveRequest,
    proofIdFromData,
 )
import Prelude hiding (id)

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
            let pid@(ProofId id) = proofIdFromData zkRequest
            status <- isProofExist conn pid
            if not status
                then do
                    addNewProveQuery conn pid Nothing
                    atomically $ writeTQueue ctxProofQueue (id, EncryptedWD zkRequest)
                else do
                    putStrLn $ "LOG: Duplicate prove request with id " <> show pid <> " rejected"
            pure pid
    handleProveWithDelegationStrategy zkRequest (Just url) = do
        eDecryptedInput <- liftIO $ runHandler $ decryptInput ctxServerKeys zkRequest :: Handler (Either ServerError i)
        case eDecryptedInput of
            Left err ->
                liftIO $ withResource ctxConnectionPool $ \conn -> do
                    let pid = proofIdFromData zkRequest
                    putStrLn $ "LOG: Error \'" <> show err <> "\' in decrypting input with id " <> show pid
                    addNewProveQuery conn pid Nothing
                    markAsFailed conn pid
                    pure pid
            Right decryptedInput -> do
                idBytes <- liftIO $ sendPostRequest (url <> "/v0/prove") (toStrict $ encode decryptedInput) customHeaders
                pid <- case decode (fromStrict idBytes) of
                    Nothing -> do
                        liftIO $ putStrLn "LOG: delegation server didn't return correct id"
                        handleProveWithDelegationStrategy zkRequest Nothing
                    Just x -> pure x
                liftIO $ withResource ctxConnectionPool $ \conn -> do
                    addNewProveQuery conn pid (Just url)
                    pure pid
