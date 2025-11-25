module ZkFold.Prover.API.Handler.Keys where

import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Servant
import ZkFold.Prover.API.Types.Ctx (Ctx (..))
import ZkFold.Prover.API.Types.Encryption (PublicKeyBundle, removePrivateKey)

-- | Type for the /keys endpoint.
type KeysEndpoint =
    Summary "Get server public keys."
        :> "keys"
        :> Get '[JSON] [PublicKeyBundle]

keysServer :: Ctx i -> Server KeysEndpoint
keysServer Ctx{..} = liftIO $ fmap (fmap removePrivateKey) (readTVarIO ctxServerKeys)
