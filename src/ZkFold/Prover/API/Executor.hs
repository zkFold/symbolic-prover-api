{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Prover.API.Executor where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString
import Data.Pool
import Servant
import ZkFold.Prover.API.Database
import ZkFold.Prover.API.Encryption
import ZkFold.Prover.API.Types.Ctx
import ZkFold.Prover.API.Types.ProveAlgorithm (ProveAlgorithm (proveAlgorithm))


proofExecutor :: forall i o. (ProveAlgorithm i o) => Ctx i -> IO ()
proofExecutor Ctx{..} = do
    _pid <- myThreadId
    forever $ do
        putStrLn "Waiting for query"
        (taskId, wd) <- atomically $ readTQueue ctxProofQueue
        eWitness <- case wd of
            Encrypted zkpr -> runHandler $ decryptInput @i ctxServerKeys zkpr
            Unencrypted w -> pure $ pure w
        case eWitness of
            Left err -> do
                putStrLn $ "LOG: Error \'" <> show err <> "\' in decrypting input with id " <> show taskId
                liftIO $ withResource ctxConnectionPool $ \conn -> do
                    markAsFailed conn taskId
            Right w -> do
                let proof = proveAlgorithm @i @o w
                let proofBytes = encode proof
                liftIO $ withResource ctxConnectionPool $ \conn -> do
                    finishTask conn taskId (toStrict proofBytes)
