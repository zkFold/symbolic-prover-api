module ZkFold.Prover.API.Handler (
  ProverAPI,
  mainApi,
  mainServer,
) where

import Control.Lens ((.~), (?~))
import Control.Lens.Lens
import Data.OpenApi (OpenApi, URL (..), applyTagsFor)
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Lens
import Data.Proxy
import GHC.Base (Symbol)
import Servant
import Servant.OpenApi
import Servant.Swagger ()
import Servant.Swagger.UI
import ZkFold.Prover.API.Encryption
import ZkFold.Prover.API.Types
import ZkFold.Prover.API.Types.Encryption ()
-- import ZkFold.Symbolic.Examples.SmartWallet (ExpModProofInput)
import Control.Monad.IO.Class
-- import Control.Monad
import ZkFold.Prover.API.Database
import Data.Pool
import ZkFold.Prover.API.Prove
type KeysEndpoint =
  Summary "Get server public keys."
    :> "keys"
    :> Get '[JSON] [PublicKeyBundle]

-- type ProveEndpoint =
--   Summary "Submit data for proving."
--     :> "prove"
--     :> ReqBody '[JSON] ZKProveRequest
--     :> Post '[JSON] ProofId

type ProofStatusEndpoint =
  Summary "Check the status of a proof."
    :> "proof-status"
    :> ReqBody '[JSON] ProofId
    :> Post '[JSON] ProofStatus

-- type ProveUnencryptedEndpoint =
--   Summary "Submit unencrypted data for proving."
--     :> "prove-unencrypted"
--     :> ReqBody '[JSON] ExpModProofInput
--     :> Post '[JSON] ProofId

type ProverEndpoints =
  KeysEndpoint
    -- :<|> ProveEndpoint
    -- :<|> ProveUnencryptedEndpoint
    :<|> ProofStatusEndpoint

type V0 ∷ Symbol
type V0 = "v0"

openApi ∷ OpenApi
openApi =
  toOpenApi proverApi
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
      (subOperations (Proxy ∷ Proxy (V0 :> KeysEndpoint)) (Proxy ∷ Proxy ProverAPI))
      ["Keys" & OpenApi.description ?~ "Endpoint to get server public keys"]
    -- & applyTagsFor
    --   (subOperations (Proxy ∷ Proxy (V0 :> ProveEndpoint)) (Proxy ∷ Proxy ProverAPI))
    --   ["Prove" & OpenApi.description ?~ "Endpoint to create task for prove"]
    -- & applyTagsFor
    --   (subOperations (Proxy ∷ Proxy (V0 :> ProveUnencryptedEndpoint)) (Proxy ∷ Proxy ProverAPI))
    --   ["Prove unencrypted" & OpenApi.description ?~ "Endpoint to create task for prove with unencrypted input"]
    & applyTagsFor
      (subOperations (Proxy ∷ Proxy (V0 :> ProofStatusEndpoint)) (Proxy ∷ Proxy ProverAPI))
      ["Proof status" & OpenApi.description ?~ "Endpoint to get information about proof status"]

type ProverAPI = V0 :> ProverEndpoints

type InfoAPI = SwaggerSchemaUI "info" "swagger.json"

type MainAPI = ProverAPI :<|> InfoAPI

handleGetKeys ∷ Ctx → Handler [PublicKeyBundle]
handleGetKeys Ctx {..} = do
  liftIO $ withResource ctxConnectionPool $ \conn -> do
    !newQueryID <- addNewProveQuery conn 1 "kkkkkkkkkk"
    print newQueryID
  getPublicKeys ctxServerKeys

-- handleProve ∷ Ctx → ZKProveRequest → Handler ProofId
-- handleProve Ctx {..} zkpr = do
--   -- !newQueryID <- liftIO $ addNewProveQuery ctxConnectionDatabase 1 ""
--   -- liftIO $ print newQueryID
--   prove ctxProofsDatabase ctxServerKeys zkpr

-- handleProveUnencrypted ∷ Ctx → ExpModProofInput → Handler ProofId
-- handleProveUnencrypted Ctx {..} = proveUnencrypted ctxProofsDatabase

handleProofStatus ∷ Ctx → ProofId → Handler ProofStatus
handleProofStatus Ctx {..} = getProofStatus ctxProofsDatabase

proverApi ∷ Proxy ProverAPI
proverApi = Proxy ∷ Proxy ProverAPI

handleProverApi ∷ Ctx → Servant.Server ProverAPI
handleProverApi ctx =
  handleGetKeys ctx
    -- :<|> handleProve ctx
    -- :<|> handleProveUnencrypted ctx
    :<|> handleProofStatus ctx

mainApi ∷ Proxy MainAPI
mainApi = Proxy

mainServer ∷ Ctx → Servant.Server MainAPI
mainServer ctx = handleProverApi ctx :<|> swaggerSchemaUIServerT openApi
