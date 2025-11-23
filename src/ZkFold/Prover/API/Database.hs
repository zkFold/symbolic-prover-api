{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Prover.API.Database where

import Control.Applicative

import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Lazy (fromStrict)
import Data.Int
import Data.Maybe
import Data.Time (UTCTime)
import Data.UUID
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField (FromField (fromField), fieldData, returnError)
import Database.SQLite.Simple.ToField (ToField (toField))
import GHC.Generics
import ZkFold.Prover.API.Handler.Delegation (customHeaders, sendPostRequest)
import ZkFold.Prover.API.Types.Prove hiding (ProofStatus (..))
import ZkFold.Prover.API.Types.Prove qualified as P
import ZkFold.Prover.API.Types.Stats (ProverStats (..))
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
    , createTime :: UTCTime
    , proofBytes :: Maybe ByteString
    , proofTime :: Maybe UTCTime
    , delegation :: Maybe String
    }
    deriving (Generic, Show)

instance FromRow Record where
    fromRow = do
        queryUUID <- field
        status <- field
        createTime <- field
        proofBytes <- field
        proofTime <- field
        delegation <- field
        pure $ Record{..}

createQueryTable :: Query
createQueryTable =
    " \
    \ CREATE TABLE IF NOT EXISTS prove_request_table ( \
    \     query_uuid varchar(36) PRIMARY KEY NOT NULL, \
    \     status varchar(16) NOT NULL, \
    \     create_time TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \     proof_bytes BLOB, \
    \     proof_time TEXT, \
    \     delegation TEXT \
    \ ); \
    \ "

initDatabase :: Connection -> IO ()
initDatabase conn = do
    void $ execute_ conn createQueryTable

addNewProveQuery :: Connection -> UUID -> Maybe String -> IO ()
addNewProveQuery conn uuid delegation = do
    void $
        execute
            conn
            " \
            \ INSERT INTO \
            \     prove_request_table ( \
            \         query_uuid, \
            \         status, \
            \         delegation \
            \     ) \
            \ VALUES (?, 'QUEUED', ?) \
            \ "
            (uuid, delegation)

getProofStatus ::
    forall o.
    (FromJSON o, ToJSON o) =>
    Connection -> ProofId -> IO (P.ProofStatus o)
getProofStatus conn (ProofId uuid) = do
    statuses <-
        query
            conn
            " \
            \ SELECT \
            \     status, \
            \     proof_bytes, \
            \     proof_time, \
            \     delegation \
            \ FROM prove_request_table \
            \ WHERE \
            \     query_uuid = ? \
            \ "
            (Only uuid)
    case statuses of
        [] -> pure $ mapToProofStatus (NotFound, Nothing)
        (status, mProof, mTime, mDelegation) : _ -> do
            let res = do
                    proofJson <- mProof
                    time <- mTime
                    proof <- decode proofJson
                    pure $ ZKProveResult proof time

            case (mDelegation, mProof) of
                (Just url, Nothing) -> do
                    response <- sendPostRequest (url <> "/v0/proof-status") (toStrict $ encode uuid) customHeaders
                    case response of
                        Left err -> do
                            putStrLn $ "LOG: Error \'" <> show err <> "\' in get proof status from delegation server " <> url <> " for delegated prove " <> show uuid
                            pure $ mapToProofStatus (status, res)
                        Right resultBytes -> do
                            case decode (fromStrict resultBytes) of
                                Nothing -> do
                                    putStrLn $ "LOG: Error in decoding proof status from delegation server " <> url <> " for delegated prove " <> show uuid
                                    pure $ mapToProofStatus (status, res)
                                Just result -> result <$ updateStatus conn uuid result
                _ -> pure $ mapToProofStatus (status, res)
  where
    mapToProofStatus = \case
        (NotFound, Nothing) -> P.NotFound
        (Pending, Nothing) -> P.Pending
        (Queued, Nothing) -> P.Queued
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
            \     create_time, \
            \     proof_bytes, \
            \     proof_time, \
            \     delegation \
            \ FROM prove_request_table \
            \ WHERE \
            \     query_uuid = ? \
            \ "
            (Only uuid)
    pure record

showStatusName :: forall o. P.ProofStatus o -> String
showStatusName = \case
    (P.Completed _) -> "COMPLETED"
    P.Failed -> "FAILED"
    P.NotFound -> "NOT-FOUND"
    P.Pending -> "PENDING"
    P.Queued -> "QUEUED"

updateStatus :: forall o. (ToJSON o) => Connection -> UUID -> P.ProofStatus o -> IO ()
updateStatus conn uuid status = do
    void $
        execute
            conn
            " \
            \ UPDATE prove_request_table \
            \ SET status = ? \
            \ WHERE query_uuid = ?; \
            \ "
            (showStatusName status, uuid)

    case status of
        P.Completed bytes ->
            void $
                execute
                    conn
                    " \
                    \ UPDATE prove_request_table \
                    \ SET proof_bytes = ?, \
                    \     proof_time = CURRENT_TIMESTAMP  \
                    \ WHERE query_uuid = ?; \
                    \ "
                    (toStrict $ encode bytes, uuid)
        _ -> pure ()

markAsPending :: Connection -> UUID -> IO ()
markAsPending conn uuid = updateStatus @() conn uuid P.Pending

markAsFailed :: Connection -> UUID -> IO ()
markAsFailed conn uuid = updateStatus @() conn uuid P.Failed

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

getProverStats :: Connection -> IO ProverStats
getProverStats conn = do
    [Only psTotalValidProofs] <-
        query_
            conn
            " \
            \ SELECT COUNT(*) \
            \ FROM prove_request_table \
            \ WHERE create_time >= datetime('now', '-1 day'); \
            \ "

    [Only psLongestQueueSize] <-
        query_
            conn
            " \
            \ SELECT \
            \   COALESCE(MAX(QueueLengthAtCreateTime), 0)\
            \ FROM \
            \   ( \
            \     SELECT \
            \       t1.create_time, \
            \       ( \
            \         SELECT  COUNT(*) \
            \         FROM  prove_request_table AS t2 \
            \         WHERE \
            \           (t2.create_time <= t1.create_time) AND \
            \           (( t2.proof_time > t1.create_time) OR (t2.proof_time IS NULL)) \
            \       )  AS QueueLengthAtCreateTime\
            \     FROM \
            \       prove_request_table  AS t1 \
            \     WHERE \
            \       t1.create_time >= datetime('now', '-1 day') \
            \   ) \
            \ "

    [Only psAverageProofTimeSeconds] <-
        query_
            conn
            " \
            \ SELECT \
            \   COALESCE(AVG( \
            \     (JULIANDAY(proof_time) - JULIANDAY(create_time)) * 86400 \
            \   ), 0.0) \
            \ FROM \
            \   prove_request_table \
            \ WHERE \
            \   proof_time IS NOT NULL \
            \   AND create_time >= datetime('now', '-1 day'); \
            \ "

    pure $ ProverStats{..}
