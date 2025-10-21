{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Prover.API.Database where

import Control.Applicative

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Int
import Data.Maybe
import Data.OpenApi
import Data.UUID
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField (FromField (fromField), returnError, fieldData)
import GHC.Generics
import ZkFold.Prover.API.Types.Prove
import ZkFold.Prover.API.Utils
import Prelude hiding (id)
import Data.Time (UTCTime)
import System.IO.Unsafe (unsafePerformIO)
import Database.SQLite.Simple.ToField (ToField (toField))

data Status = Pending | Completed | Failed
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

instance FromField Status where
    fromField f = unsafePerformIO $ do
        print $ fieldData f
        pure $ case fieldData f of
            SQLText text -> case text of
                "PENDING" -> pure Pending
                "COMPLETED" -> pure Completed
                "FAILED" -> pure Failed
                status -> returnError ConversionFailed f ("Unexpected status: " <> show status)
            sqlData -> returnError ConversionFailed f ("Unexpected data in status: " <> show sqlData)

instance FromField UUID where
    fromField f = unsafePerformIO $ do
        print $ fieldData f
        pure $ case fieldData f of
            SQLText uuidText -> case fromText uuidText of
                Just uuid -> pure uuid
                Nothing -> returnError ConversionFailed f ("Incorrect uuid format: " <> show uuidText)
            sqlData -> returnError ConversionFailed f ("Unexpected data in uuid field: " <> show sqlData)

instance ToField UUID where
    toField = SQLText . toText


data Record = Record
    { id :: Int
    , queryUUID :: UUID
    , status :: Status
    , contractId :: Int
    , createTime :: UTCTime
    , proofBytes :: Maybe ByteString
    , proofTime :: Maybe UTCTime
    }
    deriving (Generic, Show)

instance ToSchema Status where
    declareNamedSchema =
        genericDeclareNamedSchema defaultSchemaOptions
            & addSwaggerDescription "Status of a submitted proof"

instance FromRow Record where
    fromRow = do
        id <- field
        queryUUID <- field
        status <- field
        contractId <- field
        createTime <- field
        proofBytes <- field
        proofTime <- field
        pure $ Record{..}

createQueryTable :: Query
createQueryTable =
    " \
    \ CREATE TABLE IF NOT EXISTS prove_request_table ( \
    \     id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \     query_uuid varchar(36) NOT NULL, \
    \     status varchar(16) NOT NULL, \
    \     contract_id INT NOT NULL, \
    \     create_time TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \     proof_bytes BLOB, \
    \     proof_time TEXT \
    \ ); \
    \ "

initDatabase :: Connection -> IO ()
initDatabase conn = do
    void $ execute_ conn createQueryTable

addNewProveQuery :: Connection -> Int -> UUID -> IO Int
addNewProveQuery conn contractId uuid = do
    [Only result] <-
        query
            conn
            " \
            \ INSERT INTO \
            \     prove_request_table ( \
            \         query_uuid, \
            \         status, \
            \         contract_id \
            \     ) \
            \ VALUES (?, 'PENDING', ?) \
            \ RETURNING id; \
            \ "
            (uuid, contractId)
    pure result

getProofStatus ::
    forall o.
    (FromJSON o) =>
    Connection -> ProofId -> IO (Status, Maybe (ZKProveResult o))
getProofStatus conn (ProofId uuid) = do
    [(status, mProof, mTime)] <-
        query
            conn
            " \
            \ SELECT \
            \     status, \
            \     proof_bytes, \
            \     proof_time \
            \ FROM prove_request_table \
            \ WHERE \
            \     query_uuid = ? \
            \ "
            (Only uuid)

    pure
        ( status
        , do
            proofJson <- mProof
            time <- mTime
            proof <- decode proofJson
            pure $ ZKProveResult proof time
        )

getRecord :: Connection -> Int -> IO Record
getRecord conn id = do
    [record] <-
        query
            conn
            " \
            \ SELECT \
            \     id, \
            \     query_uuid, \
            \     status, \
            \     contract_id, \
            \     create_time, \
            \     proof_bytes, \
            \     proof_time \
            \ FROM prove_request_table \
            \ WHERE \
            \     id = ? \
            \ "
            (Only id)
    pure record

markAsFailed :: Connection -> Int -> IO ()
markAsFailed conn id = do
    void $
        execute
            conn
            " \
            \ UPDATE prove_request_table \
            \ SET status = 'FAILED' \
            \ WHERE id = ?; \
            \ "
            (Only id)

finishTask ::
    Connection ->
    Int ->
    ByteString ->
    IO ()
finishTask conn id proofBytes = do
    void $
        execute
            conn
            " \
            \ UPDATE prove_request_table \
            \ SET status = 'COMPLETED', \
            \     proof_bytes = ?, \
            \     proof_time = CURRENT_TIMESTAMP  \
            \ WHERE id = ?; \
            \ "
            (proofBytes, id)
