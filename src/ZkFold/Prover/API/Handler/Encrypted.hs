{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Prover.API.Handler.Encrypted where

import Control.Concurrent.STM (atomically, writeTQueue)
import Control.Lens ((?~))
import Control.Lens.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Data
import Data.Pool
import Data.Swagger
import Data.UUID.V4 (nextRandom)
import Servant
import Servant.Swagger (HasSwagger (toSwagger), subOperations)
import Servant.Swagger.UI
import ZkFold.Prover.API.Database
import ZkFold.Prover.API.Encryption
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

handleProve :: forall i. Ctx i -> ZKProveRequest -> Handler ProofId
handleProve Ctx{..} zkpr = do
    liftIO $ withResource ctxConnectionPool $ \conn -> do
        uuid <- nextRandom
        addNewProveQuery conn uuid
        atomically $ writeTQueue ctxProofQueue (uuid, EncryptedWD zkpr)
        pure $ ProofId uuid

handleProverApi :: forall i o. (FromJSON o) => Ctx i -> Servant.Server (V0 :> ProverEncryptedEndpoints i o)
handleProverApi ctx =
    handleProofStatus ctx
        :<|> handleGetKeys ctx
        :<|> handleProve ctx
        :<|> handleStats ctx

mainApi :: forall i o. Proxy (MainAPI (V0 :> ProverEncryptedEndpoints i o))
mainApi = Proxy

mainServer :: forall i o. (ProveAlgorithm i o) => Ctx i -> Servant.Server (MainAPI (V0 :> ProverEncryptedEndpoints i o))
mainServer ctx =
    handleProverApi @i @o ctx
        :<|> swaggerSchemaUIServerT (openApi @i @o)
        :<|> handleRobots
