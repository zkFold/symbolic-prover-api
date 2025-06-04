{-# LANGUAGE TypeOperators #-}

module ZkFold.Prover.API.Server (Env(..), runK, app) where

import           Control.Lens                               ((&), (.~), (?~))
import           Control.Monad.IO.Class                     (MonadIO (..))
import           Data.Maybe                                 (fromJust)
import           Data.Swagger                               hiding (get, put)
import qualified Katip                                      as K
import           Prelude
import           Servant
import           Servant.Swagger                            (HasSwagger (..))
import           Servant.Swagger.UI                         ()

import           ZkFold.Data.ByteString                     (fromByteString,
                                                             toByteString)
import           ZkFold.Protocol.NonInteractiveProof
import           ZkFold.Protocol.NonInteractiveProof.Prover (ProofBytes (..),
                                                             ProveAPIResult (..))
import           ZkFold.Prover.API.Types.Args               (WitnessBytes (..))
import           ZkFold.Prover.API.Types.ZkProof

type API = ProveAPI :<|> DocAPI

newtype Env = Env { logger :: K.LogEnv }

type ProveAPI = "prove"
  :> ReqBody '[JSON] InputBytes
  :> Post '[JSON] ProveAPIResult

runK :: MonadIO m => K.LogEnv -> K.LogStr -> m ()
runK le mes = K.runKatipContextT le () "loop" $ K.logLocM K.InfoS mes

proveHandler :: Env -> WitnessBytes -> Handler ProveAPIResult
proveHandler (Env env) (WitnessBytes bsW) = do
  runK env "Start prove"
  let setup' = setupEqualityCheckContract
  let witness' = fromJust $ fromByteString bsW
  let proveRes = prove @(PlonkupExample 16) setup' witness'
  let res = ProveAPISuccess . ProofBytes $ toByteString proveRes
  runK env $ K.logStr $ "Setup: " ++ show setup'
  runK env $ K.logStr $ "Witness: " ++ show witness'
  runK env $ K.logStr $ "ProveAPi result: " ++ show res
  pure res

type DocAPI = "swagger.json" :> Get '[JSON] Swagger

docHandler :: Swagger
docHandler = toSwagger (Proxy @ProveAPI)
  & info.title .~ "Prover API Swagger reference"
  & info.version .~ "1.0"
  & info.description ?~ "..."
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

app :: Env -> Application
app env = serve (Proxy :: Proxy API) (proveHandler env :<|> pure docHandler)
