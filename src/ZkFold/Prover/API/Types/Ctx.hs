module ZkFold.Prover.API.Types.Ctx (
    Ctx (..),
    WitnessData (..),
) where

import Control.Concurrent.STM (TQueue, TVar)

import Data.Pool
import Database.PostgreSQL.Simple
import ZkFold.Protocol.NonInteractiveProof
import ZkFold.Prover.API.Types.Encryption
import ZkFold.Prover.API.Types.Prove

data WitnessData nip = Encrypted ZKProveRequest | Unencrypted (Witness nip)

-- | Server context: configuration & shared state.
data Ctx nip = Ctx
    { ctxConnectionPool :: Pool Connection
    , ctxServerKeys :: !(TVar [KeyPair])
    , ctxProofQueue :: TQueue (Int, WitnessData nip)
    , ctxContractId :: Int
    }
