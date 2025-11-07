module ZkFold.Prover.API.Types.Status where

import Data.Aeson
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import GHC.Generics
import Data.Swagger
import ZkFold.Prover.API.Utils
import Data.Function ((&))


data Status = Pending | Completed | Failed
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

instance ToSchema Status where
    declareNamedSchema =
        genericDeclareNamedSchema defaultSchemaOptions
            & addSwaggerDescription "Status of a submitted proof"

instance FromField Status where
    fromField f = case fieldData f of
        SQLText text -> case text of
            "PENDING" -> pure Pending
            "COMPLETED" -> pure Completed
            "FAILED" -> pure Failed
            status -> returnError ConversionFailed f ("Unexpected status: " <> show status)
        sqlData -> returnError ConversionFailed f ("Unexpected data in status: " <> show sqlData)
