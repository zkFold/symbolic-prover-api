{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Prover.API.Orphans () where

import Control.Lens ((?~))
import Control.Monad.IO.Class (MonadIO (..))
import Crypto.PubKey.RSA qualified as RSA
import Crypto.Random.Types qualified as Crypto
import Data.Function ((&))
import Data.Swagger
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema (named)
import Deriving.Aeson
import Servant
import ZkFold.Prover.API.Types.Prove (ProofStatus)
import ZkFold.Prover.API.Utils (addSwaggerDescription)
import ZkFold.Prover.API.Types.Status (Status)

instance Crypto.MonadRandom Handler where
    getRandomBytes = liftIO . Crypto.getRandomBytes

instance ToSchema RSA.PublicKey where
    declareNamedSchema _ =
        pure $
            named "PublicKey" $
                mempty
                    & description
                        ?~ "JSON representation of an RSA public key"

deriving instance Generic RSA.PublicKey

data ProofStatusSchema o = ProofStatusSchema
    { contents :: o
    , tag :: Status
    }
    deriving (Generic)

instance (Swagger.ToSchema o) => Swagger.ToSchema (ProofStatusSchema o) where
    declareNamedSchema =
        Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
            & addSwaggerDescription "Status of a submitted proof"

instance (Swagger.ToSchema o) => Swagger.ToSchema (ProofStatus o) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (ProofStatusSchema o))
