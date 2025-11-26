module ZkFold.Prover.API.Handler.ProveUnencrypted where

import Control.Concurrent.STM (atomically, writeTQueue)
import Control.Monad.IO.Class (liftIO)
import Data.Pool (withResource)
import Data.UUID.V4 (nextRandom)
import Servant
import ZkFold.Prover.API.Database (addNewProveQuery)
import ZkFold.Prover.API.Types (ProofId (..))
import ZkFold.Prover.API.Types.Ctx (Ctx (..), WitnessData (..))

type ProveUnencryptedEndpoint i =
    Summary "Submit unencrypted data for proving."
        :> "prove"
        :> ReqBody '[JSON] i
        :> Post '[JSON] ProofId

proveUnencryptedServer :: forall i. Ctx i -> Server (ProveUnencryptedEndpoint i)
proveUnencryptedServer Ctx{..} = handler
  where
    handler :: i -> Handler ProofId
    handler witness = liftIO $ withResource ctxConnectionPool $ \conn -> do
        uuid <- nextRandom
        addNewProveQuery conn uuid
        atomically $ writeTQueue ctxProofQueue (uuid, UnencryptedWD witness)
        pure $ ProofId uuid
