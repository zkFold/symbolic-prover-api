module ZkFold.Prover.API.Handler.Stats where

import Control.Monad.IO.Class (liftIO)
import Data.Pool (withResource)
import Servant
import ZkFold.Prover.API.Database (getProverStats)
import ZkFold.Prover.API.Types.Ctx (Ctx (..))
import ZkFold.Prover.API.Types.Stats (ProverStats)

-- | Type for the /stats endpoint
type StatsEndpoint =
    Summary "Get prover statistics."
        :> "stats"
        :> Get '[JSON] ProverStats

statsServer :: Ctx i -> Server StatsEndpoint
statsServer Ctx{..} = liftIO $ withResource ctxConnectionPool getProverStats
