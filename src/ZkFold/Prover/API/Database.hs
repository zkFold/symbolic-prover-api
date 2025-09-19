{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultilineStrings #-}

module ZkFold.Prover.API.Database where

import Control.Applicative

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Int
import Data.Maybe
import Data.OpenApi
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.UUID
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField (fromField), returnError)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Time
import GHC.Generics
import ZkFold.Prover.API.Types.Prove
import ZkFold.Prover.API.Utils
import Prelude hiding (id)

data Status = Pending | Completed | Failed
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

instance FromField Status where
    fromField f mdata = do
        case unpack $ decodeUtf8 $ fromJust mdata of
            "PENDING" -> pure Pending
            "COMPLETED" -> pure Completed
            "FAILED" -> pure Failed
            status -> returnError ConversionFailed f ("Unexpected status: " <> status)

data Record = Record
    { id :: Int
    , queryUUID :: UUID
    , status :: Status
    , contractId :: Int
    , createTime :: UTCTimestamp
    , proofBytes :: Maybe ByteString
    , proofTime :: Maybe UTCTimestamp
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

createQueryStatusType :: Query
createQueryStatusType =
    """
    DO $$
    BEGIN
      IF NOT EXISTS (SELECT 1 FROM PG_TYPE WHERE TYPNAME = 'status') THEN
        CREATE TYPE STATUS AS ENUM ('PENDING', 'COMPLETED', 'FAILED');
      END IF;
    END $$;
    """

createQueryTable :: Query
createQueryTable =
    """
    CREATE TABLE IF NOT EXISTS prove_request_table (
        id SERIAL PRIMARY KEY,
        query_uuid uuid NOT NULL DEFAULT gen_random_uuid(),
        status status NOT NULL,
        contract_id INT NOT NULL,
        create_time TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        proof_bytes BYTEA,
        proof_time TIMESTAMPTZ
    );
    """

addNewProveQuery :: Connection -> Int -> IO (Int, UUID)
addNewProveQuery conn contractId = do
    [result] <-
        query
            conn
            """
            INSERT INTO
                prove_request_table (
                    status,
                    contract_id
                )
            VALUES ('PENDING', ?)
            RETURNING id, query_uuid;
            """
            (Only contractId)
    pure result

getProofStatus ::
    forall o.
    (FromJSON o) =>
    Connection -> ProofId -> IO (Status, Maybe (ZKProveResult o))
getProofStatus conn (ProofId uuid) = do
    [(status, mProof, mTime)] <-
        query
            conn
            """
            SELECT
                status,
                proof_bytes,
                proof_time
            FROM prove_request_table
            WHERE
                query_uuid = ?
            """
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
            """
            SELECT
                id,
                query_uuid,
                status,
                contract_id,
                create_time,
                proof_bytes,
                proof_time
            FROM prove_request_table
            WHERE
                id = ?
            """
            (Only id)
    pure record

markAsFailed :: Connection -> Int -> IO ()
markAsFailed conn id = do
    void $
        execute
            conn
            """
            UPDATE prove_request_table
            SET status = 'FAILED'
            WHERE id = ?;
            """
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
            """
            UPDATE prove_request_table
            SET status = 'COMPLETED',
                proof_bytes = ?,
                proof_time = NOW()
            WHERE id = ?;
            """
            (proofBytes, id)
