{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module ZkFold.Prover.API.Types.Common (
  ProveRequestMonad,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Crypto.Random.Types qualified as Crypto
import Servant

type ProveRequestMonad m = (MonadError ServerError m, MonadIO m, Crypto.MonadRandom m)
