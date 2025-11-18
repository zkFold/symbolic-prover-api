{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Prover.API.Database where

import Control.Applicative

import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Int
import Data.Maybe
import Data.Time (UTCTime)
import Data.UUID
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField (FromField (fromField), fieldData, returnError)
import Database.SQLite.Simple.ToField (ToField (toField))
import GHC.Generics
import ZkFold.Prover.API.Types.Prove hiding (ProofStatus (..))
import ZkFold.Prover.API.Types.Prove qualified as P
import ZkFold.Prover.API.Types.Status
import Prelude hiding (id)

instance FromField UUID where
    fromField f = case fieldData f of
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
    , createTime :: UTCTime
    , proofBytes :: Maybe ByteString
    , proofTime :: Maybe UTCTime
    }
    deriving (Generic, Show)

instance FromRow Record where
    fromRow = do
        id <- field
        queryUUID <- field
        status <- field
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
    \     create_time TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \     proof_bytes BLOB, \
    \     proof_time TEXT \
    \ ); \
    \ "

initDatabase :: Connection -> IO ()
initDatabase conn = do
    void $ execute_ conn createQueryTable

addNewProveQuery :: Connection -> UUID -> IO Int
addNewProveQuery conn uuid = do
    [Only result] <-
        query
            conn
            " \
            \ INSERT INTO \
            \     prove_request_table ( \
            \         query_uuid, \
            \         status \
            \     ) \
            \ VALUES (?, 'QUEUED') \
            \ RETURNING id; \
            \ "
            (Only uuid)
    pure result

getProofStatus ::
    forall o.
    (FromJSON o) =>
    Connection -> ProofId -> IO (P.ProofStatus o)
getProofStatus conn (ProofId uuid) = do
    statuses <-
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
    let (status, mResult) =
            case statuses of
                [] -> (NotFound, Nothing)
                (status', mProof, mTime) : _ -> (status', res)
                  where
                    res = do
                        proofJson <- mProof
                        time <- mTime
                        proof <- decode proofJson
                        pure $ ZKProveResult proof time

    pure $ case (status, mResult) of
        (NotFound, Nothing) -> P.NotFound
        (Pending, Nothing) -> P.Pending
        (Queued, Nothing) -> P.Queued
        (Completed, Just result) -> P.Completed result
        (Failed, _) -> P.Failed
        _ -> P.NotFound

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
            \     create_time, \
            \     proof_bytes, \
            \     proof_time \
            \ FROM prove_request_table \
            \ WHERE \
            \     id = ? \
            \ "
            (Only id)
    pure record

markAsPending :: Connection -> Int -> IO ()
markAsPending conn id = do
    void $
        execute
            conn
            " \
            \ UPDATE prove_request_table \
            \ SET status = 'PENDING' \
            \ WHERE id = ?; \
            \ "
            (Only id)

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
