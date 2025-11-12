{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Prover.API.Executor where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (SomeException, handle)
import Control.Monad
import Data.Aeson
import Data.ByteString
import Data.Pool
import Servant (runHandler)
import ZkFold.Prover.API.Database
import ZkFold.Prover.API.Encryption
import ZkFold.Prover.API.Orphans ()
import ZkFold.Prover.API.Types.Ctx
import ZkFold.Prover.API.Types.ProveAlgorithm (ProveAlgorithm (proveAlgorithm))

proofExecutor :: forall i o. (ProveAlgorithm i o) => Ctx i -> IO ()
proofExecutor Ctx{..} = do
    _pid <- myThreadId
    forever $ do
        putStrLn "Waiting for query"
        (taskId, wd) <- atomically $ readTQueue ctxProofQueue
        withResource ctxConnectionPool $ \conn -> markAsPending conn taskId
        eWitness <- case wd of
            Encrypted zkpr -> runHandler $ decryptInput @i ctxServerKeys zkpr
            Unencrypted w -> pure $ pure w
        case eWitness of
            Left err -> do
                putStrLn $ "LOG: Error \'" <> show err <> "\' in decrypting input with id " <> show taskId

                withResource ctxConnectionPool $ \conn -> do
                    markAsFailed conn taskId
            Right w -> handle handler $ do
                let proof = proveAlgorithm @i @o w
                let proofBytes = encode proof

                withResource ctxConnectionPool $ \conn -> do
                    finishTask conn taskId (toStrict proofBytes)
              where
                handler :: SomeException -> IO ()
                handler (ex :: SomeException) = do
                    putStrLn $ "LOG: Caught exception in executor thread: " <> show ex
                    withResource ctxConnectionPool $ \conn -> markAsFailed conn taskId
