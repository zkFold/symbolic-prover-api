module ZkFold.Prover.API.Types.Ctx (
    Ctx (..),
    WitnessData (..),
    ProverMode (..),
) where

import Control.Concurrent.STM (TQueue, TVar)

import Data.Aeson
import Data.Pool
import Database.SQLite.Simple
import ZkFold.Prover.API.Types.Encryption
import ZkFold.Prover.API.Types.Prove

data WitnessData w = EncryptedWD ZKProveRequest | UnencryptedWD w

data ProverMode = Encrypted | Plain
    deriving (Eq, Show)

instance ToJSON ProverMode where
    toJSON Encrypted = "encrypted"
    toJSON Plain = "plain"

instance FromJSON ProverMode where
    parseJSON = withText "ProverMode" f
      where
        f "encrypted" = pure Encrypted
        f "plain" = pure Plain
        f _ = fail "Unexpected encryption mode"

-- | Server context: configuration & shared state.
data Ctx w = Ctx
    { ctxConnectionPool :: Pool Connection
    , ctxServerKeys :: !(TVar [KeyPair])
    , ctxProofQueue :: TQueue (Int, WitnessData w)
    , ctxProverMode :: ProverMode
    }
