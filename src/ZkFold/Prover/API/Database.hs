{-# LANGUAGE MultilineStrings #-}

module ZkFold.Prover.API.Database where

import Control.Applicative
-- import Data.Maybe

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (readInt)
import Data.Int
import Data.Maybe
import Data.Pool
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField (fromField), returnError)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Notification
import Database.PostgreSQL.Simple.Time
import GHC.Generics
import GHC.IO (unsafePerformIO)
import ZkFold.Prover.API.Types.Ctx
import ZkFold.Prover.API.Types.Prove ()
import Prelude (IO, Num ((*)), Semigroup (..), Show (show), String, error, print, putStrLn, read, ($), (++))
import Data.UUID

data Status = Pending | Active | Completed | Failed
  deriving (Generic, Show)

instance FromField Status where
  fromField _ mdata = do
    pure $ case unpack $ decodeUtf8 $ fromJust mdata of
      "PENDING" → Pending
      "ACTIVE" → Active
      "COMPLETED" → Completed
      "FAILED" → Failed
      status → error ("Unexpected status: " <> status)

data Record = Record
  { id ∷ Int
  , status ∷ Status
  , contractId ∷ Int
  , inputBytes ∷ ByteString
  , createTime ∷ UTCTimestamp
  , proofBytes ∷ Maybe ByteString
  , proofTime ∷ Maybe UTCTimestamp
  }
  deriving (Generic, Show)

instance FromRow Record where
  fromRow = do
    id ← field
    status ← field
    contractId ← field
    inputBytes ← field
    createTime ← field
    proofBytes ← field
    proofTime ← field
    pure $ Record {..}

createQueryStatusType ∷ Query
createQueryStatusType =
  """
  DO $$
  BEGIN
    IF NOT EXISTS (SELECT 1 FROM PG_TYPE WHERE TYPNAME = 'status') THEN
      CREATE TYPE STATUS AS ENUM ('PENDING', 'ACTIVE', 'COMPLETED', 'FAILED');
    END IF;
  END $$;
  """

createQueryTable ∷ Query
createQueryTable =
  """
  CREATE TABLE IF NOT EXISTS prove_request_table (
      id SERIAL PRIMARY KEY,
      status status NOT NULL,
      contract_id INT NOT NULL,
      input_bytes BYTEA,
      create_time TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      proof_bytes BYTEA,
      proof_time TIMESTAMPTZ
  );
  """

createTriggerFunction ∷ Query
createTriggerFunction =
  """
  CREATE OR REPLACE FUNCTION notify_task_inserted()
    RETURNS trigger AS $$
  DECLARE
  BEGIN
    PERFORM pg_notify(
      'new_task_channel',
      (NEW.id)::text);
    RETURN NEW;
  END;
  $$ LANGUAGE plpgsql;
  """

createTrigger ∷ Query
createTrigger =
  """
  CREATE OR REPLACE TRIGGER new_task_notify
  AFTER INSERT ON prove_request_table
  FOR EACH ROW
  EXECUTE PROCEDURE notify_task_inserted();
  """

addNewProveQuery ∷ Connection → Int → ByteString → IO Int
addNewProveQuery conn contractId inputData = do
  [Only id] ←
    query
      conn
      """
      INSERT INTO
          prove_request_table (
              status,
              contract_id,
              input_bytes
          )
      VALUES ('PENDING', ?, ?)
      RETURNING id;
      """
      (contractId, inputData)
  pure id

getRecord ∷ Connection → Int → IO Record
getRecord conn id = do
  [record] ←
    query
      conn
      """
      SELECT
          id,
          status,
          contract_id,
          input_bytes,
          create_time,
          proof_bytes,
          proof_time
      FROM prove_request_table
      WHERE
          id = ?
      """
      (Only id)
  pure record

markAsActive ∷ Connection → Int → IO ()
markAsActive conn id = do
  void $
    execute
      conn
      """
      UPDATE prove_request_table
      SET status = 'ACTIVE'
          -- proof_bytes = null,
          -- proof_time = NOW()
      WHERE id = ?;
      """
      (Only id)

markAsFailed ∷ Connection → Int → IO ()
markAsFailed conn id = do
  void $
    execute
      conn
      """
      UPDATE prove_request_table
      SET status = 'FAILED'
          -- proof_bytes = null,
          -- proof_time = NOW()
      WHERE id = ?;
      """
      (Only id)

finishTask
  ∷ Connection
  → Int
  → ByteString
  → IO ()
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

taskManager ∷ Pool Connection → TQueue Int → IO ()
taskManager pool queue = do
  withResource pool $ \conn → do
    !_ ← execute_ conn "LISTEN new_task_channel"
    putStrLn "Task Manager: Subscribed to channel 'new_task_channel'."

    forever $ do
      (Notification pid channel msg) ← getNotification conn
      case readInt msg of
        Just (id, _) → atomically $ writeTQueue queue id
        Nothing → error "Unexpected notification from database"

proofExecutor ∷ Ctx → IO ()
proofExecutor Ctx {..} = do
  pid ← myThreadId
  forever $ do
    taskId ← atomically $ readTQueue ctxProofQueue

    inputBytes ← withResource ctxConnectionPool $ \conn → do
      markAsActive conn taskId
      getRecord conn taskId

    -- TODO: run prove
    threadDelay $ 10 * 1000000

    withResource ctxConnectionPool $ \conn → do
      finishTask conn taskId "Proof bytes example"
