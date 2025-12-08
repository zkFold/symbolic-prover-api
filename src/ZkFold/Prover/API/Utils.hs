{-# LANGUAGE FlexibleContexts #-}

module ZkFold.Prover.API.Utils (
    addSwaggerDescription,
    (!?),
) where

import Control.Lens (mapped, (?~))
import Data.Swagger

{- | Utility function to add swagger description to a schema.

FIXME This is copypaste from https://github.com/geniusyield/atlas/blob/9a20624356d13baceffe53cf2afcbeec170d5867/src/GeniusYield/Swagger/Utils.hs#L24
Importing this single function from GeniusYield will result in this server depending on all Cardano libraries
-}
addSwaggerDescription ::
    (Functor f1, Functor f2, HasSchema b1 a, HasDescription a (Maybe b2)) => b2 -> f1 (f2 b1) -> f1 (f2 b1)
addSwaggerDescription desc = mapped . mapped . schema . description ?~ desc

-- | Copied from base for compatibility with ghc 9.6.7
(!?) :: [a] -> Int -> Maybe a
{-# INLINEABLE (!?) #-}
xs !? n
    | n < 0 = Nothing
    | otherwise =
        foldr
            ( \x r k -> case k of
                0 -> Just x
                _ -> r (k - 1)
            )
            (const Nothing)
            xs
            n