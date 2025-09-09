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
import Data.OpenApi.Internal.Schema
import Data.OpenApi.Lens
import Data.Pool
import GHC.Base (Symbol)
import Servant
import Servant.OpenApi
import Servant.Swagger ()
import Servant.Swagger.UI
import ZkFold.Protocol.NonInteractiveProof.Class
import ZkFold.Prover.API.Database
import ZkFold.Prover.API.Encryption
import ZkFold.Prover.API.Types
import ZkFold.Prover.API.Types.Encryption ()
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

type ProofStatusEndpoint nip =
    Summary "Check the status of a proof."
        :> "proof-status"
        :> ReqBody '[JSON] ProofId
        :> Post '[JSON] (Status, Maybe (ZKProveResult nip))

type ProveUnencryptedEndpoint nip =
    Summary "Submit unencrypted data for proving."
        :> "prove-unencrypted"
        :> ReqBody '[JSON] (Witness nip)
        :> Post '[JSON] ProofId

type ProverEndpoints nip =
    KeysEndpoint
        :<|> ProveEndpoint
        :<|> ProveUnencryptedEndpoint nip
        :<|> ProofStatusEndpoint nip

type V0 :: Symbol
type V0 = "v0"

openApi :: forall nip. (Typeable nip, ToSchema (Witness nip), ToSchema (Proof nip)) => OpenApi
openApi =
    toOpenApi (proverApi @nip)
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
            (subOperations (Proxy :: Proxy (V0 :> ProveUnencryptedEndpoint nip)) (Proxy :: Proxy (ProverAPI nip)))
            ["Keys" & OpenApi.description ?~ "Endpoint to create task for prove with unencrypted input"]
        & applyTagsFor
            (subOperations (Proxy :: Proxy (V0 :> KeysEndpoint)) (Proxy :: Proxy (ProverAPI nip)))
            ["Keys" & OpenApi.description ?~ "Endpoint to get server public keys"]
        & applyTagsFor
            (subOperations (Proxy :: Proxy (V0 :> ProveEndpoint)) (Proxy :: Proxy (ProverAPI nip)))
            ["Prove" & OpenApi.description ?~ "Endpoint to create task for prove"]
        & applyTagsFor
            (subOperations (Proxy :: Proxy (V0 :> ProofStatusEndpoint nip)) (Proxy :: Proxy (ProverAPI nip)))
            ["Proof status" & OpenApi.description ?~ "Endpoint to get information about proof status"]

type ProverAPI nip = V0 :> ProverEndpoints nip

type InfoAPI = SwaggerSchemaUI "info" "swagger.json"

type MainAPI nip = ProverAPI nip :<|> InfoAPI

handleGetKeys :: forall nip. Ctx nip -> Handler [PublicKeyBundle]
handleGetKeys Ctx{..} = getPublicKeys ctxServerKeys

handleProve :: forall nip. Ctx nip -> ZKProveRequest -> Handler ProofId
handleProve Ctx{..} zkpr = do
    liftIO $ withResource ctxConnectionPool $ \conn -> do
        (id, uuid) <- addNewProveQuery conn ctxContractId
        atomically $ writeTQueue ctxProofQueue (id, Encrypted zkpr)
        pure $ ProofId uuid

handleProofStatus :: forall nip. (FromJSON (Proof nip)) => Ctx nip -> ProofId -> Handler (Status, Maybe (ZKProveResult nip))
handleProofStatus Ctx{..} pid = liftIO $ withResource ctxConnectionPool $ \conn -> getProofStatus @nip conn pid

handleProveUnencrypted :: forall nip. Ctx nip -> Witness nip -> Handler ProofId
handleProveUnencrypted Ctx{..} w = do
    liftIO $ withResource ctxConnectionPool $ \conn -> do
        (id, uuid) <- addNewProveQuery conn ctxContractId
        atomically $ writeTQueue ctxProofQueue (id, Unencrypted w)
        pure $ ProofId uuid

proverApi :: forall nip. Proxy (ProverAPI nip)
proverApi = Proxy :: Proxy (ProverAPI nip)

handleProverApi :: forall nip. (FromJSON (Proof nip)) => Ctx nip -> Servant.Server (ProverAPI nip)
handleProverApi ctx =
    handleGetKeys ctx
        :<|> handleProve ctx
        :<|> handleProveUnencrypted ctx
        :<|> handleProofStatus ctx

mainApi :: forall nip. Proxy (MainAPI nip)
mainApi = Proxy

mainServer :: forall nip. (FromJSON (Proof nip), Typeable nip, ToSchema (Witness nip), ToSchema (Proof nip)) => Ctx nip -> Servant.Server (MainAPI nip)
mainServer ctx = handleProverApi @nip ctx :<|> swaggerSchemaUIServerT (openApi @nip)
