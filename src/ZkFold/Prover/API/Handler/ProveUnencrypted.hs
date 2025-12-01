module ZkFold.Prover.API.Handler.ProveUnencrypted where

import Control.Concurrent.STM (atomically, writeTQueue)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Pool (withResource)
import Data.UUID.V4 (nextRandom)
import Servant
import ZkFold.Prover.API.Database (addNewProveQuery)
import ZkFold.Prover.API.Handler.Delegation (customHeaders, delegationStrategy, sendPostRequest)
import ZkFold.Prover.API.Orphans ()
import ZkFold.Prover.API.Types (ProofId (..))
import ZkFold.Prover.API.Types.Ctx (Ctx (..), WitnessData (..))

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
            uuid <- nextRandom
            addNewProveQuery conn uuid Nothing
            atomically $ writeTQueue ctxProofQueue (uuid, UnencryptedWD w)
            pure $ ProofId uuid
    handleProveWithDelegationStrategy w (Just url) = do
        uuidBytes <- liftIO $ sendPostRequest (url <> "/v0/prove") (toStrict $ encode w) customHeaders
        ProofId uuid <- case decode (fromStrict uuidBytes) of
            Nothing -> do
                liftIO $ putStrLn "LOG: delegation server didn't return correct uuid"
                handleProveWithDelegationStrategy w Nothing
            Just x -> pure x
        liftIO $ withResource ctxConnectionPool $ \conn -> do
            addNewProveQuery conn uuid (Just url)
            pure $ ProofId uuid