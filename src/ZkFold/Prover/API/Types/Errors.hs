{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module ZkFold.Prover.API.Types.Errors (
  ZKKeyError (..),
  ZKProofError (..),
  ZKDecryptionError (..),
  ZKProverException (..),
  throw,
) where

import Control.Exception (Exception)
import Control.Monad.Error.Class (MonadError (..))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Text qualified as Text
import GHC.Generics
import Servant

-- | Errors related to encryption keys
data ZKKeyError
  = -- | Provided key ID is not found.
    ZKInvalidKeyID !Text.Text
  deriving stock (Generic, Show)

data ZKProofError
  = -- | Provided key ID is not found.
    ZKInvalidProofID !Text.Text
  deriving stock (Generic, Show)

-- | Errors related to decrypting payload
data ZKDecryptionError
  = -- | Payload could not be decrypted and parsed.
    ZKDecryptionFailed !Text.Text
  deriving stock (Generic, Show)

data ZKProverException
  = -- | Errors related to encryption keys
    ZKPEKeyError !ZKKeyError
  | -- | Errors related to proofs
    ZKPEProofError !ZKProofError
  | -- | Errors related to decrypting payload
    ZKPEDecryptionError !ZKDecryptionError
  deriving stock Show
  deriving anyclass Exception

throw :: MonadError ServerError m ⇒ ZKProverException → m a
throw e = throwError $ err400 {errBody = BS.fromStrict $ C8.pack (show e)}
