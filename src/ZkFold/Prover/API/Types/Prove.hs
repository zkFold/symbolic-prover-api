module ZkFold.Prover.API.Types.Prove (
  ZKProveRequest (..),
  ZKProveResult (..),
  ProofStatus (..),
  ProofQueue,
  ProofId,
  randomProofId,
  proofIdToText,
  Proofs,
) where

-- import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.OpenApi
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID, toText)
import Data.UUID.V4 (nextRandom)
import Deriving.Aeson
-- import ZkFold.Symbolic.Examples.SmartWallet (ByteStringFromHex (..), ZKProofBytes (..))

import ZkFold.Prover.API.Orphans ()
import ZkFold.Prover.API.Types.Encryption (KeyID)
import ZkFold.Prover.API.Utils
import Control.Concurrent.STM

data ZKProveRequest
  = ZKProveRequest
  { preqKeyId ∷ KeyID
  , preqAES ∷ String
  , preqPayload ∷ String
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema ZKProveRequest where
  declareNamedSchema =
    genericDeclareNamedSchema defaultSchemaOptions
      & addSwaggerDescription "Data required for ZK proof"

-- | Proof bytes with their creation time.
-- It might be useful if we decide to remove old results.
data ZKProveResult
  = ZKProveResult
  { presBytes ∷ String
  , presTimestamp ∷ UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema ZKProveResult where
  declareNamedSchema =
    genericDeclareNamedSchema defaultSchemaOptions
      & addSwaggerDescription "ZK proof bytes with a timestamp"

data ProofStatus = Pending | Completed ZKProveResult
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema ProofStatus where
  declareNamedSchema =
    genericDeclareNamedSchema defaultSchemaOptions
      & addSwaggerDescription "Status of a submitted proof"

newtype ProofId = ProofId UUID
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

instance ToSchema ProofId where
  declareNamedSchema =
    genericDeclareNamedSchema defaultSchemaOptions
      & addSwaggerDescription "ID of a submitted prove request"

randomProofId ∷ MonadIO m ⇒ m ProofId
randomProofId = ProofId <$> liftIO nextRandom

proofIdToText ∷ ProofId → Text
proofIdToText (ProofId pid) = toText pid

type ProofQueue = TChan ProofId

-- | A shared Map with data about proofs accessible by their IDs.
-- If an ID isn't in the Map, there was no proof with such ID registered.
-- If the value associated with a given ID is Nothing, the proof hasn't finished yet.
-- Otherwise, the Map will contain Proof bytes.
type Proofs = TVar (Map ProofId (Maybe ZKProveResult))
