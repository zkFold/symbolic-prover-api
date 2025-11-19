{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module ZkFold.Prover.API.Handler.Unencrypted where

import Control.Concurrent.STM (atomically, writeTQueue)
import Control.Lens ((?~))
import Control.Lens.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Data
import Data.Pool
import Data.Swagger (HasDescription (description), Swagger, applyTagsFor)
import Data.UUID.V4 (nextRandom)
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import ZkFold.Prover.API.Database
import ZkFold.Prover.API.Handler.General (MainAPI, ProofStatusEndpoint, V0, baseOpenApi, handleProofStatus)
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

openApi :: forall i o. (ProveAlgorithm i o) => Swagger
openApi =
    baseOpenApi (toSwagger proxy)
        & applyTagsFor
            (subOperations proxy proxy)
            ["ZK prover endpoints" & description ?~ "Submit a proof and get proof status."]
  where
    proxy = Proxy :: Proxy (V0 :> ProverUnencryptedEndpoint i o)

handleProve :: forall i. Ctx i -> i -> Handler ProofId
handleProve Ctx{..} w = do
    liftIO $ withResource ctxConnectionPool $ \conn -> do
        uuid <- nextRandom
        id <- addNewProveQuery conn uuid
        atomically $ writeTQueue ctxProofQueue (id, UnencryptedWD w)
        pure $ ProofId uuid

handleProverApi :: forall i o. (FromJSON o) => Ctx i -> Servant.Server (V0 :> ProverUnencryptedEndpoint i o)
handleProverApi ctx =
    handleProofStatus ctx
        :<|> handleProve ctx

mainApi :: forall i o. Proxy (MainAPI (V0 :> ProverUnencryptedEndpoint i o))
mainApi = Proxy

mainServer :: forall i o. (ProveAlgorithm i o) => Ctx i -> Servant.Server (MainAPI (V0 :> ProverUnencryptedEndpoint i o))
mainServer ctx =
    handleProverApi @i @o ctx
        :<|> swaggerSchemaUIServerT (openApi @i @o)
        :<|> handleRobots
