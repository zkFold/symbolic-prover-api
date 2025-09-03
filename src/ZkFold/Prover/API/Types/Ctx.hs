module ZkFold.Prover.API.Types.Ctx (
  Ctx (..),
) where

import Control.Concurrent.STM (TVar, TQueue)

import ZkFold.Prover.API.Types.Encryption
import ZkFold.Prover.API.Types.Prove
import Database.PostgreSQL.Simple
import Data.Pool

-- | Server context: configuration & shared state.
data Ctx = Ctx
  { ctxProofsDatabase ∷ !Proofs
  , ctxConnectionPool :: Pool Connection 
  , ctxServerKeys ∷ !(TVar [KeyPair])
  , ctxProofQueue :: TQueue Int
  }
