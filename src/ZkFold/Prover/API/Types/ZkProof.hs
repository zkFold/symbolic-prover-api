{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Prover.API.Types.ZkProof where

import           Data.Binary
import           Data.ByteString                            (ByteString)
import           Data.Swagger                               hiding (get, put)
import           GHC.Generics                               (Par1, U1 (..),
                                                             (:*:) (..))
import           GHC.TypeNats
import           Poly                                       (RustPolyVec)
import           Prelude                                    hiding (Bool, (==))
import           Types                                      (Rust_BLS12_381_G1_Point,
                                                             Rust_BLS12_381_G2_Point)
import           ZkFold.Algebra.Class                       (FromConstant (..))
import           ZkFold.Algebra.EllipticCurve.Class
import           ZkFold.Protocol.NonInteractiveProof
import           ZkFold.Protocol.NonInteractiveProof.Prover (ProofBytes (..),
                                                             ProveAPIResult (..))

import           RustBLS                                    ()
import           ZkFold.Protocol.Plonkup
import           ZkFold.Protocol.Plonkup.Input
import           ZkFold.Protocol.Plonkup.Proof
import           ZkFold.Protocol.Plonkup.Prover
import           ZkFold.Protocol.Plonkup.Utils
import           ZkFold.Protocol.Plonkup.Witness
import           ZkFold.Prover.API.Types.Args               (WitnessBytes (..))
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Compiler
import           ZkFold.Symbolic.Compiler.ArithmeticCircuit
import           ZkFold.Symbolic.Data.Bool                  (Bool)
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.FieldElement

instance
  forall f i g1 .
  ( f ~ ScalarFieldOf g1
  , Binary (i f)
  ) => Binary (PlonkupWitnessInput i g1) where
  put PlonkupWitnessInput{..} = put witnessInput
  get = PlonkupWitnessInput <$> get

instance (Binary (ScalarFieldOf g1))
  => Binary (PlonkupProverSecret g1) where
  put (PlonkupProverSecret a) = put a
  get = PlonkupProverSecret <$> get

instance (Binary (ScalarFieldOf g1))
  => Binary (PlonkupInput g1) where
  put = put . unPlonkupInput
  get = PlonkupInput <$> get

instance
  ( Binary g1
  , Binary (ScalarFieldOf g1)
  ) => Binary (PlonkupProof g1) where
  put PlonkupProof{..} = put cmA <> put cmB <> put cmC <> put cmF <> put cmH1 <> put cmH2 <> put cmZ1 <> put cmZ2 <> put cmQlow <> put cmQmid <> put cmQhigh <> put proof1 <> put proof2 <> put a_xi <> put b_xi <> put c_xi <> put s1_xi <> put s2_xi <> put f_xi <> put t_xi <> put t_xi' <> put z1_xi' <> put z2_xi' <> put h1_xi' <> put h2_xi <> put l1_xi <> put l_xi
  get = PlonkupProof <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

instance ToSchema ProofBytes where
  declareNamedSchema _ = pure $ NamedSchema (Just "Proof bytes") byteSchema

instance ToSchema ProveAPIResult where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

type InputBytes = WitnessBytes

type PlonkupExample n = Plonkup Par1 Par1 n
  Rust_BLS12_381_G1_Point
  Rust_BLS12_381_G2_Point
  ByteString
  (RustPolyVec (ScalarFieldOf Rust_BLS12_381_G1_Point))

equalityCheckContract :: forall c . (Symbolic c) => FieldElement c -> FieldElement c -> Bool c
equalityCheckContract targetValue inputValue = inputValue == targetValue

setupEqualityCheckContract :: SetupProve (PlonkupExample 16)
setupEqualityCheckContract = setupProve @(PlonkupExample 16) plonk
  where
    ac = compileWith solder (\i ->
      (U1 :*: U1, i :*: U1))
      (equalityCheckContract (fromConstant (0 :: Natural)))
    (omega, k1, k2) = getParams 16
    x = fromConstant (5 :: Natural)
    (gs, h1) = getSecretParams  x
    plonk = Plonkup omega k1 k2 ac h1 gs :: PlonkupExample 16
