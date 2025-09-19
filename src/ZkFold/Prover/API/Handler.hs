{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Prover.API.Handler (
    ProverAPI,
    mainApi,
    mainServer,
) where

import Control.Concurrent.STM (atomically, writeTQueue)
import Control.Lens ((.~), (?~))
import Control.Lens.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Data
import Data.OpenApi (OpenApi, URL (..), applyTagsFor)
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Lens
import Data.Pool
import GHC.Base (Symbol)
import Servant
import Servant.OpenApi
import Servant.Swagger ()
import Servant.Swagger.UI
import ZkFold.Prover.API.Database
import ZkFold.Prover.API.Encryption
import ZkFold.Prover.API.Types
import ZkFold.Prover.API.Types.Encryption ()
import ZkFold.Prover.API.Types.ProveAlgorithm (ProveAlgorithm)
import Prelude hiding (id)

type KeysEndpoint =
    Summary "Get server public keys."
        :> "keys"
        :> Get '[JSON] [PublicKeyBundle]

type ProveEndpoint =
    Summary "Submit data for proving."
        :> "prove"
        :> ReqBody '[JSON] ZKProveRequest
        :> Post '[JSON] ProofId

type ProofStatusEndpoint o =
    Summary "Check the status of a proof."
        :> "proof-status"
        :> ReqBody '[JSON] ProofId
        :> Post '[JSON] (Status, Maybe (ZKProveResult o))

type ProveUnencryptedEndpoint i =
    Summary "Submit unencrypted data for proving."
        :> "prove-unencrypted"
        :> ReqBody '[JSON] i
        :> Post '[JSON] ProofId

type ProverEndpoints i o =
    KeysEndpoint
        :<|> ProveEndpoint
        :<|> ProveUnencryptedEndpoint i
        :<|> ProofStatusEndpoint o

type V0 :: Symbol
type V0 = "v0"

openApi :: forall i o. (ProveAlgorithm i o) => OpenApi
openApi =
    toOpenApi (proverApi @i @o)
        & info
            . OpenApi.title
            .~ "zkFold Smart Wallet Server API"
        & info
            . version
            .~ "0.0.1"
        & info
            . license
            ?~ ("Apache-2.0" & url ?~ URL "https://opensource.org/licenses/apache-2-0")
        & info
            . contact
            ?~ ( mempty
                    & url
                        ?~ URL "https://zkfold.io/"
                    & email
                        ?~ "info@zkfold.io"
                    & name
                        ?~ "zkFold Technical Support"
               )
        & info
            . OpenApi.description
            ?~ "API to interact with zkFold Smart Wallet Prover Server"
        & applyTagsFor
            (subOperations (Proxy :: Proxy (V0 :> ProveUnencryptedEndpoint i)) (Proxy :: Proxy (ProverAPI i o)))
            ["Keys" & OpenApi.description ?~ "Endpoint to create task for prove with unencrypted input"]
        & applyTagsFor
            (subOperations (Proxy :: Proxy (V0 :> KeysEndpoint)) (Proxy :: Proxy (ProverAPI i o)))
            ["Keys" & OpenApi.description ?~ "Endpoint to get server public keys"]
        & applyTagsFor
            (subOperations (Proxy :: Proxy (V0 :> ProveEndpoint)) (Proxy :: Proxy (ProverAPI i o)))
            ["Prove" & OpenApi.description ?~ "Endpoint to create task for prove"]
        & applyTagsFor
            (subOperations (Proxy :: Proxy (V0 :> ProofStatusEndpoint o)) (Proxy :: Proxy (ProverAPI i o)))
            ["Proof status" & OpenApi.description ?~ "Endpoint to get information about proof status"]

type ProverAPI i o = V0 :> ProverEndpoints i o

type InfoAPI = SwaggerSchemaUI "info" "swagger.json"

type MainAPI i o = ProverAPI i o :<|> InfoAPI

handleGetKeys :: forall i. Ctx i -> Handler [PublicKeyBundle]
handleGetKeys Ctx{..} = getPublicKeys ctxServerKeys

handleProve :: forall i. Ctx i -> ZKProveRequest -> Handler ProofId
handleProve Ctx{..} zkpr = do
    liftIO $ withResource ctxConnectionPool $ \conn -> do
        (id, uuid) <- addNewProveQuery conn ctxContractId
        atomically $ writeTQueue ctxProofQueue (id, Encrypted zkpr)
        pure $ ProofId uuid

handleProofStatus :: forall i o. (FromJSON o) => Ctx i -> ProofId -> Handler (Status, Maybe (ZKProveResult o))
handleProofStatus Ctx{..} pid = liftIO $ withResource ctxConnectionPool $ \conn -> getProofStatus @o conn pid

handleProveUnencrypted :: forall i. Ctx i -> i -> Handler ProofId
handleProveUnencrypted Ctx{..} w = do
    liftIO $ withResource ctxConnectionPool $ \conn -> do
        (id, uuid) <- addNewProveQuery conn ctxContractId
        atomically $ writeTQueue ctxProofQueue (id, Unencrypted w)
        pure $ ProofId uuid

proverApi :: forall i o. Proxy (ProverAPI i o)
proverApi = Proxy :: Proxy (ProverAPI i o)

handleProverApi :: forall i o. (FromJSON o) => Ctx i -> Servant.Server (ProverAPI i o)
handleProverApi ctx =
    handleGetKeys ctx
        :<|> handleProve ctx
        :<|> handleProveUnencrypted ctx
        :<|> handleProofStatus ctx

mainApi :: forall i o. Proxy (MainAPI i o)
mainApi = Proxy

mainServer :: forall i o. (ProveAlgorithm i o) => Ctx i -> Servant.Server (MainAPI i o)
mainServer ctx = handleProverApi @i @o ctx :<|> swaggerSchemaUIServerT (openApi @i @o)
