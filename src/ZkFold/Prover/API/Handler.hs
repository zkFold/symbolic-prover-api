{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Prover.API.Handler where

import Control.Lens ((.~), (?~))
import Control.Lens.Lens
import Data.Swagger (Swagger, URL (..), applyTagsFor)
import Data.Swagger.Lens
import Servant
import Servant.Swagger (HasSwagger (toSwagger), subOperations)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServerT)
import ZkFold.Prover.API.Orphans ()
import ZkFold.Prover.API.Handler.Keys (KeysEndpoint, keysServer)
import ZkFold.Prover.API.Handler.ProofStatus (ProofStatusEndpoint, proofStatusServer)
import ZkFold.Prover.API.Handler.ProveEncrypted (ProveEncryptedEndpoint, proveEncryptedServer)
import ZkFold.Prover.API.Handler.ProveUnencrypted (ProveUnencryptedEndpoint, proveUnencryptedServer)
import ZkFold.Prover.API.Handler.Stats (StatsEndpoint, statsServer)
import ZkFold.Prover.API.Robots (RobotsAPI, handleRobots)
import ZkFold.Prover.API.Types.Ctx (Ctx)
import ZkFold.Prover.API.Types.ProveAlgorithm (ProveAlgorithm)
import GHC.Base (Symbol)
import Prelude hiding (id)

type V0 :: Symbol
type V0 = "v0"

type InfoAPI = SwaggerSchemaUI "docs" "swagger.json"

type MainAPI api = api :<|> InfoAPI :<|> RobotsAPI

baseOpenApi :: Swagger -> Swagger
baseOpenApi spec =
    spec
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

type ProverEndpoints i o =
    ProofStatusEndpoint o
        :<|> StatsEndpoint
        :<|> ProveUnencryptedEndpoint i
        :<|> KeysEndpoint
        :<|> ProveEncryptedEndpoint

type ProverAPI i o = MainAPI (V0 :> ProverEndpoints i o)

openApi :: forall i o. (ProveAlgorithm i o) => Swagger
openApi =
    baseOpenApi (toSwagger proxy)
        & applyTagsFor
            (subOperations proxy proxy)
            [ "ZK prover endpoints"
                & description
                    ?~ "Retrieve server keys, submit encrypted or unencrypted proofs, and check proof status."
            ]
  where
    proxy = Proxy :: Proxy (V0 :> ProverEndpoints i o)

api :: forall i o. Proxy (ProverAPI i o)
api = Proxy

apiServer :: forall i o. (ProveAlgorithm i o) => Ctx i -> Server (ProverAPI i o)
apiServer ctx =
    (proofStatusServer ctx
        :<|> statsServer ctx
        :<|> proveUnencryptedServer ctx
        :<|> keysServer ctx
        :<|> proveEncryptedServer ctx
    )
        :<|> swaggerSchemaUIServerT (openApi @i @o)
        :<|> handleRobots
