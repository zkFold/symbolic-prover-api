{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module ZkFold.Prover.API.Types.ProveAlgorithm where

import Data.ByteString (ByteString)
import GHC.TypeLits (KnownNat)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_JacobianPoint, BLS12_381_G2_JacobianPoint, Fr)
import ZkFold.Algebra.Polynomial.Univariate (PolyVec)
import ZkFold.FFI.Rust.Plonkup (rustPlonkupProve)
import ZkFold.Protocol.NonInteractiveProof
import ZkFold.Protocol.Plonkup (Plonkup)

class ProveAlgorithm nip where
    proveAlgorithm :: SetupProve nip -> Witness nip -> Proof nip

instance {-# OVERLAPPABLE #-} (NonInteractiveProof nip) => ProveAlgorithm nip where
    proveAlgorithm = (snd .) . (prove @nip)

instance {-# OVERLAPPING #-} (KnownNat n) => ProveAlgorithm (Plonkup i o n BLS12_381_G1_JacobianPoint BLS12_381_G2_JacobianPoint ByteString (PolyVec Fr)) where
    proveAlgorithm = (fst .) . rustPlonkupProve