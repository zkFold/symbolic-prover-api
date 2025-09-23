{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Prover.API.Types.Prove (
    ZKProveRequest (..),
    ZKProveResult (..),
    ProofId (..),
) where

import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Deriving.Aeson

import Control.Lens
import Data.Data
import Data.OpenApi
import ZkFold.Prover.API.Orphans ()
import ZkFold.Prover.API.Types.Encryption (KeyID)
import ZkFold.Prover.API.Utils

data ZKProveRequest
    = ZKProveRequest
    { preqKeyId :: KeyID
    , preqAES :: String
    , preqPayload :: String
    }
    deriving stock (Eq, Generic, Ord, Show)
    deriving anyclass (FromJSON, ToJSON)

instance ToSchema ZKProveRequest where
    declareNamedSchema =
        genericDeclareNamedSchema defaultSchemaOptions
            & addSwaggerDescription "Data required for ZK proof"

{- | Proof bytes with their creation time.
It might be useful if we decide to remove old results.
-}
data ZKProveResult o
    = ZKProveResult
    { presProof :: o
    , presTimestamp :: UTCTime
    }
    deriving stock (Generic)

instance (ToJSON o) => ToJSON (ZKProveResult o)
instance (FromJSON o) => FromJSON (ZKProveResult o)

instance forall o. (Typeable o, ToSchema o) => ToSchema (ZKProveResult o) where
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
