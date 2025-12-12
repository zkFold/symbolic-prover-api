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
import Database.SQLite.Simple
import GHC.Generics
import ZkFold.Prover.API.Handler.Delegation (customHeaders, sendPostRequest)
import ZkFold.Prover.API.Types.Prove hiding (ProofStatus (..))
import ZkFold.Prover.API.Types.Prove qualified as P
import ZkFold.Prover.API.Types.Stats (ProverStats (..))
import ZkFold.Prover.API.Types.Status
import Prelude hiding (id)


data Record = Record
    { id :: ProofId
    , status :: Status
    , createTime :: UTCTime
    , proofBytes :: Maybe ByteString
    , proofTime :: Maybe UTCTime
    , delegation :: Maybe String
    }
    deriving (Generic, Show)
    deriving anyclass (FromRow)

createQueryTable :: Query
createQueryTable =
    " \
    \ CREATE TABLE IF NOT EXISTS prove_request_table ( \
    \     id varchar(64) PRIMARY KEY NOT NULL, \
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

addNewProveQuery :: Connection -> ProofId -> Maybe String -> IO ()
addNewProveQuery conn (ProofId id) delegation = do
    void $
        execute
            conn
            " \
            \ INSERT INTO \
            \     prove_request_table ( \
            \         id, \
            \         status, \
            \         delegation \
            \     ) \
            \ VALUES (?, 'QUEUED', ?) \
            \ "
            (id, delegation)

getProofStatus ::
    forall o.
    (FromJSON o, ToJSON o) =>
    Connection -> ProofId -> IO (P.ProofStatus o)
getProofStatus conn pid@(ProofId id) = do
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
            \     id = ? \
            \ "
            (Only id)
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
                    resultBytes <- sendPostRequest (url <> "/v0/proof-status") id customHeaders
                    case decode (fromStrict resultBytes) of
                        Nothing -> do
                            putStrLn $ "LOG: Error in decoding proof status from delegation server " <> url <> " for delegated prove " <> show id
                            pure $ mapToProofStatus (status, res)
                        Just result -> result <$ updateStatus conn pid result
                _ -> pure $ mapToProofStatus (status, res)
  where
    mapToProofStatus = \case
        (NotFound, Nothing) -> P.NotFound
        (Pending, Nothing) -> P.Pending
        (Queued, Nothing) -> P.Queued
        (Completed, Just result) -> P.Completed result
        (Failed, _) -> P.Failed
        _ -> P.NotFound

getRecord :: Connection -> ProofId -> IO Record
getRecord conn (ProofId id) = do
    [record] <-
        query
            conn
            " \
            \ SELECT \
            \     id, \
            \     status, \
            \     create_time, \
            \     proof_bytes, \
            \     proof_time, \
            \     delegation \
            \ FROM prove_request_table \
            \ WHERE \
            \     id = ? \
            \ "
            (Only id)
    pure record

showStatusName :: forall o. P.ProofStatus o -> String
showStatusName = \case
    (P.Completed _) -> "COMPLETED"
    P.Failed -> "FAILED"
    P.NotFound -> "NOT-FOUND"
    P.Pending -> "PENDING"
    P.Queued -> "QUEUED"

updateStatus :: forall o. (ToJSON o) => Connection -> ProofId -> P.ProofStatus o -> IO ()
updateStatus conn (ProofId id) status = do
    void $
        execute
            conn
            " \
            \ UPDATE prove_request_table \
            \ SET status = ? \
            \ WHERE id = ?; \
            \ "
            (showStatusName status, id)

    case status of
        P.Completed res ->
            void $
                execute
                    conn
                    " \
                    \ UPDATE prove_request_table \
                    \ SET proof_bytes = ?, \
                    \     proof_time = ? \
                    \ WHERE id = ?; \
                    \ "
                    (toStrict $ encode $ presBytes res, presTimestamp res, id)
        _ -> pure ()

markAsPending :: Connection -> ProofId -> IO ()
markAsPending conn pid = updateStatus @() conn pid P.Pending

markAsFailed :: Connection -> ProofId -> IO ()
markAsFailed conn pid = updateStatus @() conn pid P.Failed

finishTask ::
    Connection ->
    ProofId ->
    ByteString ->
    IO ()
finishTask conn (ProofId id) proofBytes = do
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
