{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.ByteString (ByteString)
import Data.Swagger (NamedSchema (..), ToSchema (..))
import GHC.Generics
import Options.Applicative
import ZkFold.Algebra.Class hiding ((/))
import ZkFold.Algebra.EllipticCurve.BLS12_381
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Algebra.Number (Natural)
import ZkFold.Algebra.Polynomial.Univariate
import ZkFold.ArithmeticCircuit
import ZkFold.Data.Eq
import ZkFold.Protocol.NonInteractiveProof
import ZkFold.Protocol.Plonkup
import ZkFold.Protocol.Plonkup.Proof (PlonkupProof)
import ZkFold.Protocol.Plonkup.Prover (PlonkupProverSecret)
import ZkFold.Protocol.Plonkup.Utils
import ZkFold.Protocol.Plonkup.Witness
import ZkFold.Prover.API.Server
import ZkFold.Prover.API.Types.Config
import ZkFold.Prover.API.Types.ProveAlgorithm (ProveAlgorithm (proveAlgorithm))
import ZkFold.Symbolic.Class
import ZkFold.Symbolic.Compiler
import ZkFold.Symbolic.Data.Bool (Bool)
import ZkFold.Symbolic.Data.FieldElement
import ZkFold.Symbolic.Data.Vec
import Prelude hiding (Bool, (==))

type I = (U1 :*: U1) :*: (Par1 :*: U1)
type G1 = BLS12_381_G1_JacobianPoint
type G2 = BLS12_381_G2_JacobianPoint
type PV = (PolyVec (ScalarFieldOf G1))

type PlonkupExample n =
    Plonkup I Par1 n G1 G2 ByteString PV

instance {-# OVERLAPPING #-} ToSchema (PlonkupWitnessInput I G1) where
    declareNamedSchema _ = do
        pure $ NamedSchema (Just "PlonkupWitnessInput") mempty

equalityCheckContract :: forall c. (Symbolic c) => FieldElement c -> FieldElement c -> Bool c
equalityCheckContract targetValue inputValue = inputValue == targetValue

setupEqualityCheckContract :: SetupProve (PlonkupExample 16)
setupEqualityCheckContract = setupProve plonk
  where
    ac = runVec $ compile (equalityCheckContract (fromConstant (0 :: Natural))) :: ArithmeticCircuit Fr I Par1
    (omega, k1, k2) = getParams 16
    x = fromConstant (5 :: Natural)
    (gs, h1) = getSecretParams x
    plonk = Plonkup omega k1 k2 ac h1 gs :: PlonkupExample 16

instance ProveAlgorithm (PlonkupWitnessInput I G1, PlonkupProverSecret G1) (PlonkupProof G1) where
    proveAlgorithm = snd . prove @(PlonkupExample 16) setupEqualityCheckContract

main :: IO ()
main = do
    serverConfig <- parseConfig

    print @String ("Started with " <> show serverConfig)
    runServer @(Witness (PlonkupExample 16)) @(Proof (PlonkupExample 16)) serverConfig
