{-# LANGUAGE AllowAmbiguousTypes #-}
module ZkFold.Prover.API.Handler.Unencrypted where

import Control.Concurrent.STM (atomically, writeTQueue)
import Control.Lens ((?~))
import Control.Lens.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Data
import Data.OpenApi (OpenApi, applyTagsFor)
import Data.OpenApi qualified as OpenApi
import Data.Pool
import Data.UUID.V4 (nextRandom)
import Servant
import Servant.OpenApi
import Servant.Swagger ()
import Servant.Swagger.UI
import ZkFold.Prover.API.Database
import ZkFold.Prover.API.Handler.General (MainAPI, ProofStatusEndpoint, V0, handleProofStatus, baseOpenApi)
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

openApi :: forall i o. (ProveAlgorithm i o) => OpenApi
openApi =
    baseOpenApi (toOpenApi proxy)
        & applyTagsFor
            (subOperations (Proxy :: Proxy (V0 :> ProofStatusEndpoint o)) proxy)
            ["Proof status" & OpenApi.description ?~ "Endpoint to get information about proof status"]
        & applyTagsFor
            (subOperations (Proxy :: Proxy (V0 :> ProveUnencryptedEndpoint i)) proxy)
            ["Prove" & OpenApi.description ?~ "Endpoint to create task for prove with unencrypted input"]
  where
    proxy = Proxy :: Proxy (V0 :> ProverUnencryptedEndpoint i o)

handleProve :: forall i. Ctx i -> i -> Handler ProofId
handleProve Ctx{..} w = do
    liftIO $ withResource ctxConnectionPool $ \conn -> do
        uuid <- nextRandom
        id <- addNewProveQuery conn ctxContractId uuid
        atomically $ writeTQueue ctxProofQueue (id, Unencrypted w)
        pure $ ProofId uuid

handleProverApi :: forall i o. (FromJSON o) => Ctx i -> Servant.Server (V0 :> ProverUnencryptedEndpoint i o)
handleProverApi ctx =
    handleProofStatus ctx
        :<|> handleProve ctx

mainApi :: forall i o. Proxy (MainAPI (V0 :> ProverUnencryptedEndpoint i o))
mainApi = Proxy

mainServer :: forall i o. (ProveAlgorithm i o) => Ctx i -> Servant.Server (MainAPI (V0 :> ProverUnencryptedEndpoint i o))
mainServer ctx = handleProverApi @i @o ctx :<|> swaggerSchemaUIServerT (openApi @i @o)
