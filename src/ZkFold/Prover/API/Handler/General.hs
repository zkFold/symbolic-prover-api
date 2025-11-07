{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Prover.API.Handler.General where

import Control.Lens ((.~), (?~))
import Control.Lens.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Pool
import Data.Swagger (Swagger, URL (..))
import Data.Swagger.Lens
import GHC.Base (Symbol)
import Servant
import Servant.Swagger ()
import Servant.Swagger.UI
import ZkFold.Prover.API.Database
import ZkFold.Prover.API.Types
import ZkFold.Prover.API.Types.Encryption ()
import Prelude hiding (id)

type V0 :: Symbol
type V0 = "v0"

type InfoAPI = SwaggerSchemaUI "docs" "swagger.json"

type MainAPI api = api :<|> InfoAPI

type ProofStatusEndpoint o =
    Summary "Check the status of a proof."
        :> "proof-status"
        :> ReqBody '[JSON] ProofId
        :> Post '[JSON] (ProofStatus o)

baseOpenApi :: Swagger -> Swagger
baseOpenApi api =
    api
        & info
            . title
            .~ "zkFold Prover Server API"
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
            . description
            ?~ "API to interact with zkFold Prover Server"

handleProofStatus :: forall i o. (FromJSON o) => Ctx i -> ProofId -> Handler (ProofStatus o)
handleProofStatus Ctx{..} pid = liftIO $ withResource ctxConnectionPool $ \conn -> getProofStatus @o conn pid
