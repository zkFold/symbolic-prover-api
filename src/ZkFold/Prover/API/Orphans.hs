{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Prover.API.Orphans () where

import Control.Lens ((?~))
import Control.Monad.IO.Class (MonadIO (..))
import Crypto.PubKey.RSA qualified as RSA
import Crypto.Random.Types qualified as Crypto
import Data.Function ((&))
import Data.OpenApi
import Data.OpenApi.Internal.Schema (named)
import Deriving.Aeson
import Servant
import ZkFold.Symbolic.Examples.SmartWallet (ByteStringFromHex (..))

instance Crypto.MonadRandom Handler where
    getRandomBytes = liftIO . Crypto.getRandomBytes

instance ToSchema ByteStringFromHex where
  declareNamedSchema _ =
    pure $
      named "ByteStringFromHex" $
        mempty
          & type_
            ?~ OpenApiString
          & format
            ?~ "hex"
          & description
            ?~ "Bytes encoded in hex."

instance ToSchema RSA.PublicKey where
    declareNamedSchema _ =
        pure $
            named "PublicKey" $
                mempty
                    & description
                        ?~ "JSON representation of an RSA public key"

deriving instance Generic RSA.PublicKey
