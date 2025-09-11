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
import ZkFold.Protocol.NonInteractiveProof
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
data ZKProveResult nip
    = ZKProveResult
    { presProof :: Proof nip
    , presTimestamp :: UTCTime
    }
    deriving stock (Generic)

instance (ToJSON (Proof nip)) => ToJSON (ZKProveResult nip)
instance (FromJSON (Proof nip)) => FromJSON (ZKProveResult nip)

instance forall nip p. (Typeable nip, p ~ Proof nip, ToSchema p) => ToSchema (ZKProveResult nip) where
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
