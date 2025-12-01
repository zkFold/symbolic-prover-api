{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Prover.API.Handler.ProofStatus where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Pool (withResource)
import Servant
import ZkFold.Prover.API.Database (getProofStatus)
import ZkFold.Prover.API.Types (ProofId (..), ProofStatus)
import ZkFold.Prover.API.Types.Ctx (Ctx (..))

-- | Type for the /proof-status endpoint
type ProofStatusEndpoint o =
    Summary "Check the status of a proof."
        :> "proof-status"
        :> ReqBody '[JSON] ProofId
        :> Post '[JSON] (ProofStatus o)

proofStatusServer :: forall i o. (ToJSON o, FromJSON o) => Ctx i -> Server (ProofStatusEndpoint o)
proofStatusServer Ctx{..} = handler
  where
    handler :: ProofId -> Handler (ProofStatus o)
    handler pid = liftIO $ withResource ctxConnectionPool $ \conn -> getProofStatus @o conn pid
