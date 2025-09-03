{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Prover.API.Orphans () where

import Control.Lens ((?~))
import Control.Monad.IO.Class (MonadIO (..))
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.Random.Types as Crypto
import Data.Function ((&))
import Data.OpenApi
import Data.OpenApi.Internal.Schema (named)
import Servant
import ZkFold.Prover.API.Utils
-- import ZkFold.Symbolic.Examples.SmartWallet (ByteStringFromHex (..), ZKF (..), ZKProofBytes (..), ExpModProofInput)

instance Crypto.MonadRandom Handler where
  getRandomBytes = liftIO . Crypto.getRandomBytes

instance ToSchema RSA.PublicKey where
  declareNamedSchema _ =
    pure $
      named "PublicKey" $
        mempty
          & description
            ?~ "JSON representation of an RSA public key"

-- instance ToSchema ZKF where
--   declareNamedSchema =
--     genericDeclareNamedSchema defaultSchemaOptions
--       & addSwaggerDescription "Field element."

-- instance ToSchema ByteStringFromHex where
--   declareNamedSchema _ =
--     pure $
--       named "ByteStringFromHex" $
--         mempty
--           & type_
--             ?~ OpenApiString
--           & format
--             ?~ "hex"
--           & description
--             ?~ "Bytes encoded in hex."

-- instance ToSchema ZKProofBytes where
--   declareNamedSchema =
--     genericDeclareNamedSchema defaultSchemaOptions
--       & addSwaggerDescription "Proof bytes where bytes are represented in hexadecimal encoding."

-- instance ToSchema ExpModProofInput where