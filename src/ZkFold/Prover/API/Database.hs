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
    { queryUUID :: UUID
    , status :: Status
    , contractId :: Int
    , createTime :: UTCTime
    , proofBytes :: Maybe ByteString
    , proofTime :: Maybe UTCTime
    }
    deriving (Generic, Show)

instance FromRow Record where
    fromRow = do
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
    \     query_uuid varchar(36) PRIMARY KEY NOT NULL, \
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

addNewProveQuery :: Connection -> Int -> UUID -> IO ()
addNewProveQuery conn contractId uuid = do
    void $
        execute
            conn
            " \
            \ INSERT INTO \
            \     prove_request_table ( \
            \         query_uuid, \
            \         status, \
            \         contract_id \
            \     ) \
            \ VALUES (?, 'PENDING', ?) \
            \ "
            (uuid, contractId)

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
        (Completed, Just result) -> P.Completed result
        (Failed, _) -> P.Failed
        _ -> P.NotFound

getRecord :: Connection -> UUID -> IO Record
getRecord conn uuid = do
    [record] <-
        query
            conn
            " \
            \ SELECT \
            \     query_uuid, \
            \     status, \
            \     contract_id, \
            \     create_time, \
            \     proof_bytes, \
            \     proof_time \
            \ FROM prove_request_table \
            \ WHERE \
            \     query_uuid = ? \
            \ "
            (Only uuid)
    pure record

markAsFailed :: Connection -> UUID -> IO ()
markAsFailed conn uuid = do
    void $
        execute
            conn
            " \
            \ UPDATE prove_request_table \
            \ SET status = 'FAILED' \
            \ WHERE query_uuid = ?; \
            \ "
            (Only uuid)

finishTask ::
    Connection ->
    UUID ->
    ByteString ->
    IO ()
finishTask conn uuid proofBytes = do
    void $
        execute
            conn
            " \
            \ UPDATE prove_request_table \
            \ SET status = 'COMPLETED', \
            \     proof_bytes = ?, \
            \     proof_time = CURRENT_TIMESTAMP  \
            \ WHERE query_uuid = ?; \
            \ "
            (proofBytes, uuid)

deleteOldProofs :: Connection -> Int -> IO ()
deleteOldProofs conn days = do
    void $
        execute
            conn
            " \
            \ DELETE FROM prove_request_table \
            \ WHERE julianday('now') - julianday(create_time) >= ? \
            \ "
            (Only days)
