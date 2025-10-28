module ZkFold.Prover.API.Types.Ctx (
    Ctx (..),
    WitnessData (..),
    EncryptionMode (..),
) where

import Control.Concurrent.STM (TQueue, TVar)

import Data.Pool
import Database.SQLite.Simple
import ZkFold.Prover.API.Types.Encryption
import ZkFold.Prover.API.Types.Prove

data WitnessData w = Encrypted ZKProveRequest | Unencrypted w

data EncryptionMode = EncryptedMode | UnencryptedMode
    deriving (Eq, Show)

-- | Server context: configuration & shared state.
data Ctx w = Ctx
    { ctxConnectionPool :: Pool Connection
    , ctxServerKeys :: !(TVar [KeyPair])
    , ctxProofQueue :: TQueue (Int, WitnessData w)
    , ctxContractId :: Int
    , ctxEncryptionMode :: EncryptionMode
    }
