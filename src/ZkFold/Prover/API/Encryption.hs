module ZkFold.Prover.API.Encryption (
  getPublicKeys,
  -- decryptInput,
) where

import Control.Concurrent.STM.TVar (TVar, readTVarIO)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher (..), Cipher (..), makeIV)
import Crypto.Error (CryptoFailable (..))
import Data.Aeson (decode)
import Data.ByteString qualified as BS
import Data.Coerce (coerce)
import Data.Text qualified as T
import ZkFold.Prover.API.Types.Common
import ZkFold.Prover.API.Types.Encryption
import ZkFold.Prover.API.Types.Errors
import ZkFold.Prover.API.Types.Prove
-- import ZkFold.Symbolic.Examples.SmartWallet (ByteStringFromHex (..), ExpModProofInput (..))

-- TODO: Create a proper key storage/initialisation, implement key rotation, etc
--
getServerKeys ∷ ProveRequestMonad m ⇒ TVar [KeyPair] → m [KeyPair]
getServerKeys = liftIO . readTVarIO

getPublicKeys ∷ ProveRequestMonad m ⇒ TVar [KeyPair] → m [PublicKeyBundle]
getPublicKeys keysVar = fmap removePrivateKey <$> getServerKeys keysVar

-- | Remove PKCS#7-style padding
stripPKCS7 ∷ ProveRequestMonad m ⇒ BS.ByteString → m BS.ByteString
stripPKCS7 bs = do
  when (BS.null bs) $
    throw (ZKPEDecryptionError $ ZKDecryptionFailed "Cannot strip padding from empty plaintext")
  when (paddingLen == 0 || paddingLen > 16) $
    throw (ZKPEDecryptionError $ ZKDecryptionFailed $ "Invalid padding length: " <> T.pack (show paddingLen))
  when (BS.length bs < paddingLen) $
    throw (ZKPEDecryptionError $ ZKDecryptionFailed "Padding length exceeds plaintext length")
  unless (BS.all (== fromIntegral paddingLen) (BS.takeEnd paddingLen bs)) $
    throw (ZKPEDecryptionError $ ZKDecryptionFailed "Invalid PKCS#7 padding")
  pure $ BS.take (BS.length bs - paddingLen) bs
 where
  paddingLen = fromIntegral (BS.last bs)

-- decryptInput ∷ ProveRequestMonad m ⇒ TVar [KeyPair] → ZKProveRequest → m ExpModProofInput
-- decryptInput keysVar ZKProveRequest {..} = do
--   serverKeys ← getServerKeys keysVar
--   let matchingKeys = filter ((== preqKeyId) . kpId) serverKeys
--   when (null matchingKeys) $ throw (ZKPEKeyError (ZKInvalidKeyID $ keyIdToText preqKeyId))
--   let KeyPair {..} = case matchingKeys of
--         [k] → k
--         _ → error "impossible"

--   -- First, decode the symmetric AES key
--   aesKey ← decrypt (Just "AES key") kpPrivate (coerce preqAES)
--   unless (BS.length aesKey == 32) $
--     throw (ZKPEDecryptionError $ ZKDecryptionFailed "AES key is not 32-byte long")

--   -- Then, decode the payload itself
--   let (ivBytes, ciphertext) = BS.splitAt 16 (coerce preqPayload)
--   iv ← case makeIV ivBytes of
--     Nothing → throw (ZKPEDecryptionError $ ZKDecryptionFailed "Invalid IV")
--     Just iv' → pure iv'

--   cipher ← case cipherInit aesKey ∷ CryptoFailable AES256 of
--     CryptoFailed err → throw (ZKPEDecryptionError $ ZKDecryptionFailed $ "Cipher init failed: " <> T.pack (show err))
--     CryptoPassed cipher' → pure cipher'

--   let paddedText = cbcDecrypt cipher iv ciphertext

--   inputJSON ← stripPKCS7 paddedText

--   -- Lastly, parse the payload into JSON
--   case decode $ BS.fromStrict inputJSON of
--     Nothing →
--       throw
--         (ZKPEDecryptionError $ ZKDecryptionFailed $ "JSON decoding failed. Attempted to decode " <> (T.pack . show) inputJSON)
--     Just result → pure result
