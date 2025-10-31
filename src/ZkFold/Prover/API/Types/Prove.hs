{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module ZkFold.Prover.API.Types.Prove (
    ZKProveRequest (..),
    ZKProveResult (..),
    ProofId (..),
    ProofStatus(..)
) where

import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Deriving.Aeson

import Control.Lens
import Data.Aeson.Casing
import Data.Char (isLower)
import Data.Data
import Data.OpenApi
import Data.OpenApi qualified as Swagger
import Data.OpenApi.Declare qualified as Swagger
import Data.Text qualified as T
import ZkFold.Prover.API.Orphans ()
import ZkFold.Prover.API.Types.Common (addDescription, addFieldDescription)
import ZkFold.Prover.API.Types.Encryption (KeyID)
import ZkFold.Prover.API.Utils
import ZkFold.Symbolic.Examples.SmartWallet (ByteStringFromHex (..))

data ZKProveRequest
  = ZKProveRequest
  { preqServerKeyId ∷ KeyID
  , preqAesEncryptionKey ∷ ByteStringFromHex
  , preqEncryptedPayload ∷ ByteStringFromHex
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "preq", CamelToSnake]] ZKProveRequest

data ProofStatus o = Pending | Failed | Completed (ZKProveResult o)
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

deriving instance (Show (ZKProveResult o)) => Show (ProofStatus o)

instance (ToSchema o) => Swagger.ToSchema (ProofStatus o) where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Status of a submitted proof"

instance Swagger.ToSchema ZKProveRequest where
    declareNamedSchema _ = do
        proveRequestSchema <-
            Swagger.genericDeclareNamedSchema
                Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = snakeCase . dropWhile isLower}
                (Proxy :: Proxy ZKProveRequest)
        defs <- Swagger.look
        let inlineSchema@Swagger.NamedSchema{..} = Swagger.inlineNonRecursiveSchemas defs proveRequestSchema
        pure inlineSchema{Swagger._namedSchemaSchema = customise _namedSchemaSchema}
      where
        encryptionInstructions =
            T.unlines
                [ "Data required for ZK proof"
                , "The unencrypted payload must be a JSON of the following format:"
                , "\t"
                , "\t{                        "
                , "\t\tpiPubE: integer,       "
                , "\t\tpiPubN: integer,       "
                , "\t\tpiSignature: integer,  "
                , "\t\tpiTokenName: integer   "
                , "\t}                        "
                , "\t"
                , "piPubE is the public exponent used in the RSA key used to sign the JSON Web Token, as a decimal integer."
                , "piPubN is the public modulus used in the RSA key used to sign the JSON Web Token, as a decimal integer."
                , "piSignature is the signature attached to the JSON Web Token, as a decimal integer."
                , "piTokenName is the hash of the public key corresponding to the Wallet's root key, as a decimal integer."
                , "⋅"
                , "The Prover server uses combined encryption to secure sensitive information."
                , "Payload is encrypted with a symmetric cipher, while its encryption key is itself encrypted with the Server's 2048-bit RSA key."
                , "⋅"
                , "The JSON from above must be encrypted with AES-256-CBC and PKCS#7 padding, making the \"encrypted_payload\" field."
                , "AES-256 encryption key must itself be encrypted with one of the Prover server's public keys, making the \"aes_encryption_key\" field."
                , "The key ID of the Server's RSA key used to encrypt the AES key makes the \"server_key_id\" field."
                , "⋅"
                ]
        customise s =
            s
                & addDescription encryptionInstructions
                & addFieldDescription "server_key_id" "ZK Prover server public key ID used to encrypt the AES key."
                & addFieldDescription
                    "aes_encryption_key"
                    "Hex-encoded AES-256 key for decyphering the payload. The key should be encrypted with one of the server's RSA keys."
                & addFieldDescription "encrypted_payload" "Hex-encoded ZK Prover input encrypted with AES-256-CBC and PKCS#7 padding."

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
        Swagger.genericDeclareNamedSchema
            Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = snakeCase . dropWhile isLower}
            & addSwaggerDescription "ZK proof bytes with a timestamp"

newtype ProofId = ProofId UUID
    deriving stock (Eq, Generic, Ord, Show)
    deriving newtype (FromJSON, ToJSON)

instance ToSchema ProofId where
    declareNamedSchema =
        Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
            & addSwaggerDescription "ID of a submitted prove request"
