module ZkFold.Prover.API.Handler.ProveEncrypted where

import Control.Concurrent.STM (atomically, writeTQueue)
import Control.Monad.IO.Class (liftIO)
import Data.Pool (withResource)
import Data.UUID.V4 (nextRandom)
import Servant
import ZkFold.Prover.API.Database (addNewProveQuery)
import ZkFold.Prover.API.Types (ProofId (..))
import ZkFold.Prover.API.Types.Ctx (Ctx (..), WitnessData (..))
import ZkFold.Prover.API.Types.Prove (ZKProveRequest)

-- | Type for the encrypted prove endpoint.
type ProveEncryptedEndpoint =
    Summary "Submit encrypted data for proving."
        :> "prove-encrypted"
        :> ReqBody '[JSON] ZKProveRequest
        :> Post '[JSON] ProofId

proveEncryptedServer :: forall i. Ctx i -> Server ProveEncryptedEndpoint
proveEncryptedServer Ctx{..} = handler
  where
    handler :: ZKProveRequest -> Handler ProofId
    handler zkRequest = liftIO $ withResource ctxConnectionPool $ \conn -> do
        uuid <- nextRandom
        addNewProveQuery conn uuid
        atomically $ writeTQueue ctxProofQueue (uuid, EncryptedWD zkRequest)
        pure $ ProofId uuid
