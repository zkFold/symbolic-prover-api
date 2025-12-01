module ZkFold.Prover.API.Types.Ctx (
    Ctx (..),
    WitnessData (..),
) where

import Control.Concurrent.STM (TQueue, TVar)

import Data.Pool
import Data.UUID (UUID)
import Database.SQLite.Simple
import ZkFold.Prover.API.Types.Encryption
import ZkFold.Prover.API.Types.Prove

data WitnessData w = EncryptedWD ZKProveRequest | UnencryptedWD w

-- | Server context: configuration & shared state.
data Ctx w = Ctx
    { ctxConnectionPool :: Pool Connection
    , ctxServerKeys :: !(TVar [KeyPair])
    , ctxProofQueue :: TQueue (UUID, WitnessData w)
    , ctxDelegationServers :: [String]
    }
