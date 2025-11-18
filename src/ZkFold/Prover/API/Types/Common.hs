{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module ZkFold.Prover.API.Types.Common (
    ProveRequestMonad,
    addFieldDescription,
    addDescription,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Crypto.Random.Types qualified as Crypto
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.Swagger qualified as Swagger
import Data.Text
import Servant

type ProveRequestMonad m = (MonadError ServerError m, MonadIO m, Crypto.MonadRandom m)

-- | In a given Swagger Schema, replace description "desc" for the field named @name@, if there is one.
addFieldDescription :: Text -> Text -> Swagger.Schema -> Swagger.Schema
addFieldDescription name desc s =
    s
        { Swagger._schemaProperties =
            InsOrd.adjust (fmap (\sub -> sub{Swagger._schemaDescription = Just desc})) name $ Swagger._schemaProperties s
        }

addDescription :: Text -> Swagger.Schema -> Swagger.Schema
addDescription desc s = s{Swagger._schemaDescription = Just desc}
