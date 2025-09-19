{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ZkFold.Prover.API.Types.ProveAlgorithm where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)

class (ToSchema i, ToSchema o, ToJSON o, FromJSON o, FromJSON i) => ProveAlgorithm i o where
    proveAlgorithm :: i -> o
