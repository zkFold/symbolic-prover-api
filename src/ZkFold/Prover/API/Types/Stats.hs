module ZkFold.Prover.API.Types.Stats where

import Control.Lens ((&))
import Data.Aeson
import Data.Aeson.Casing
import Data.Char (isLower)
import Data.Swagger qualified as Swagger
import Deriving.Aeson
import ZkFold.Prover.API.Utils

-- | Prover statistics for the last 24 hours
data ProverStats = ProverStats
    { psTotalValidProofs :: Int
    , psLongestQueueSize :: Int
    , psAverageProofTimeSeconds :: Double
    }
    deriving stock (Eq, Generic, Show)
    deriving
        (FromJSON, ToJSON)
        via CustomJSON '[FieldLabelModifier '[StripPrefix "ps", CamelToSnake]] ProverStats

instance Swagger.ToSchema ProverStats where
    declareNamedSchema =
        Swagger.genericDeclareNamedSchema
            Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = snakeCase . dropWhile isLower}
            & addSwaggerDescription "Prover statistics for the last 24 hours"