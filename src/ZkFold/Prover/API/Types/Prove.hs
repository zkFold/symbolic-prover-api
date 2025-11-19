{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Prover.API.Types.Prove (
    ZKProveRequest (..),
    ZKProveResult (..),
    ProofId (..),
    ProofStatus (..),
) where

import Data.Time.Clock (UTCTime)
import Data.UUID qualified as UUID (UUID, nil, toText)
import Deriving.Aeson

import Control.Lens
import Data.Aeson (ToJSON (..))
import Data.Aeson.Casing
import Data.Char (isLower)
import Data.Data
import Data.Swagger qualified as Swagger
import Data.Swagger.Declare qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Data.Text qualified as T
import ZkFold.Prover.API.Types.Common (addDescription, addFieldDescription)
import ZkFold.Prover.API.Types.Encryption (KeyID)
import ZkFold.Prover.API.Utils
import ZkFold.Symbolic.Examples.SmartWallet (ByteStringFromHex (..))

data ZKProveRequest
    = ZKProveRequest
    { preqServerKeyId :: KeyID
    , preqAesEncryptionKey :: ByteStringFromHex
    , preqEncryptedPayload :: ByteStringFromHex
    }
    deriving stock (Eq, Generic, Ord, Show)
    deriving
        (FromJSON, ToJSON)
        via CustomJSON '[FieldLabelModifier '[StripPrefix "preq", CamelToSnake]] ZKProveRequest

data ProofStatus o = Completed (ZKProveResult o) | Pending | Queued | Failed | NotFound
    deriving stock (Generic)
    deriving anyclass (FromJSON, ToJSON)

deriving instance (Show (ZKProveResult o)) => Show (ProofStatus o)

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
                , "The JSON from above must be encrypted with AES-256-CBC and Optimal Asymmetric Encryption Padding(OAEP), making the \"encrypted_payload\" field."
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
    { presBytes :: o
    , presTimestamp :: UTCTime
    }
    deriving stock (Generic, Show)
    deriving
        (FromJSON, ToJSON)
        via CustomJSON '[FieldLabelModifier '[StripPrefix "pres", CamelToSnake]] (ZKProveResult o)

instance forall o. (Swagger.ToSchema o) => Swagger.ToSchema (ZKProveResult o) where
    declareNamedSchema =
        Swagger.genericDeclareNamedSchema
            Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = snakeCase . dropWhile isLower}
            & addSwaggerDescription "ZK proof bytes with a timestamp"

newtype ProofId = ProofId UUID.UUID
    deriving stock (Eq, Generic, Ord, Show)
    deriving newtype (FromJSON, ToJSON)

instance Swagger.ToSchema ProofId where
    declareNamedSchema _ =
        pure $
            Swagger.named "UUID" $
                mempty
                    & Swagger.type_ ?~ Swagger.SwaggerString
                    & Swagger.format ?~ "string"
                    & Swagger.example ?~ toJSON ("\"" <> UUID.toText UUID.nil <> "\"")
                    & Swagger.description ?~ "ID of a submitted prove request"