module ZkFold.Prover.API.Handler.ProveUnencrypted where

import Control.Concurrent.STM (atomically, writeTQueue)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Pool (withResource)
import Servant
import ZkFold.Prover.API.Database (addNewProveQuery)
import ZkFold.Prover.API.Handler.Delegation (customHeaders, delegationStrategy, sendPostRequest)
import ZkFold.Prover.API.Orphans ()
import ZkFold.Prover.API.Types (ProofId (..))
import ZkFold.Prover.API.Types.Ctx (Ctx (..), WitnessData (..))
import ZkFold.Prover.API.Types.Prove (proofIdFromData)
import Prelude hiding (id)

type ProveUnencryptedEndpoint i =
    Summary "Submit unencrypted data for proving."
        :> "prove"
        :> ReqBody '[JSON] i
        :> Post '[JSON] ProofId

proveUnencryptedServer :: forall i. (ToJSON i) => Ctx i -> Server (ProveUnencryptedEndpoint i)
proveUnencryptedServer Ctx{..} = handler
  where
    handler :: i -> Handler ProofId
    handler witness = do
        strategy <- delegationStrategy ctxDelegationServers witness
        handleProveWithDelegationStrategy witness strategy

    handleProveWithDelegationStrategy :: i -> Maybe String -> Handler ProofId
    handleProveWithDelegationStrategy w Nothing =
        liftIO $ withResource ctxConnectionPool $ \conn -> do
            let pid@(ProofId id) = proofIdFromData w
            addNewProveQuery conn pid Nothing
            atomically $ writeTQueue ctxProofQueue (id, UnencryptedWD w)
            pure pid
    handleProveWithDelegationStrategy w (Just url) = do
        idBytes <- liftIO $ sendPostRequest (url <> "/v0/prove") (toStrict $ encode w) customHeaders
        pid <- case decode (fromStrict idBytes) of
            Nothing -> do
                liftIO $ putStrLn "LOG: delegation server didn't return correct id"
                handleProveWithDelegationStrategy w Nothing
            Just x -> pure x
        liftIO $ withResource ctxConnectionPool $ \conn -> do
            addNewProveQuery conn pid (Just url)
            pure pid