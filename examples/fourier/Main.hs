{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

import qualified Blaze.ByteString.Builder.Char8 as Blaze
import qualified Data.ByteString.Lazy as ByteString
import Data.Foldable (Foldable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid ((<>))
import Data.Traversable (Traversable)
import qualified Numeric
import qualified System.IO as IO

import Control.Monad.Ask.Class (MonadAsk)
import Control.Monad.Memo.Class
import Control.Monad.Quantum.CircuitBuilderToffoli
import Control.Monad.Quantum.Class
import Control.Monad.Quantum.Counter
import Control.Monad.Quantum.PhaseShift.Binary.Counter
import Control.Monad.Quantum.PhaseShift.Binary.Kickback
import Control.Monad.Quantum.PhaseShift.Binary.Kickback.Estimate
import Control.Monad.Quantum.PhaseShift.Binary.Kickback.ResourceState
import Control.Monad.Quantum.PhaseShift.Binary.FourierTransform
import Control.Monad.Quantum.PhaseShift.CircuitBuilder
import Control.Monad.Quantum.PhaseShift.SQCT
import Data.Quantum.Circuit (IsGate(..), Gate, IsCircuit, Circuit)
import Data.Quantum.Circuit.QC (GateToQCBuilder(..), runQCBuilder)
import Data.Quantum.Cost.Width
import Data.Quantum.Cost.TotalGates
import Data.Quantum.PhaseShift.GatePhaseShift
import Data.Quantum.PhaseShift.Binary.Stat
import Data.Quantum.SQCT
import Data.Quantum.Wire
import Data.Void
import CommandLine
import Util (showsWithCommas, showsIntegerWithCommas)

data MyGate w
  = MGGate !(Gate w)
  | MGPhaseShift !(GatePhaseShift w)
  deriving (Functor, Foldable, Traversable)

instance Show w => Show (MyGate w) where
  showsPrec d (MGGate g) = showsPrec d g
  showsPrec d (MGPhaseShift g) = showsPrec d g

instance IsGate MyGate where
  gateX w = MGGate $ gateX w
  gateXC w w1 s1 = MGGate $ gateXC w w1 s1
  gateXCC w w1 s1 w2 s2 = MGGate $ gateXCC w w1 s1 w2 s2
  gateH w = MGGate $ gateH w
  gateY w = MGGate $ gateY w
  gateZ w = MGGate $ gateZ w
  gateS w inv = MGGate $ gateS w inv
  gateT w inv = MGGate $ gateT w inv
  invertGate (MGGate e) = MGGate (invertGate e)
  invertGate (MGPhaseShift e) = MGPhaseShift (invertGatePhaseShift e)

instance IsGatePhaseShift MyGate where
  gatePhaseShift w theta = MGPhaseShift $ gatePhaseShift w theta

instance GateToQCBuilder MyGate where
  gateToQCBuilder (MGGate g) =
      gateToQCBuilder g
  gateToQCBuilder (MGPhaseShift (GatePhaseShift w fraction)) =
      Blaze.fromString "RZ(" <> Blaze.fromString (Numeric.showFFloat Nothing (fraction * 2) "") <>
      Blaze.fromString ") " <> Blaze.fromShow w

reportSolovayKitaevCost :: (forall m. MonadBinaryPhaseShift () m => m ()) -> HashMap Rational [SQSolution] -> ShowS
reportSolovayKitaevCost circuit sqdb =
    showsResult
  where
    ~(_, BinaryPhaseShiftStat phaseKickbackDegree maxParallelPhaseKickbacks) =
        runQuantumCounterWithoutMemo (runBinaryPhaseShiftCounterT circuit) 0 ()
    ~(resourceStatePrecision, phaseShiftFactors, errorPerApproxSequence) =
        chooseResourceStatePrecision phaseKickbackDegree 1e-3
    approxSequences =
        length phaseShiftFactors
    -- circuit2: circuit after implementing phase kickback
    circuit2 :: forall m. (MonadAsk (HashMap Rational SQSolution) m, MonadQuantum () m, MonadMemo (Either Void (Int, Integer)) m) => m ()
    circuit2 = do
        _resource <- runPhaseShiftSQCTT $ prepareResourceStates phaseKickbackDegree resourceStatePrecision maxParallelPhaseKickbacks
        runPhaseKickbackEstimateT circuit
    sqdbFiltered =
        filterSolutionDatabase errorPerApproxSequence sqdb
    ~(_, ~(width, totalGates)) =
        runQuantumCounter circuit2 0 sqdbFiltered
    showsResult =
        showString "Phase kickback degree for the whole circuit: " . shows phaseKickbackDegree . showChar '\n' .
        showString "Phase kickbacks in parallel: " . showsIntegerWithCommas maxParallelPhaseKickbacks . showChar '\n' .
        showString "Phase kickback resource state precision: " . showsWithCommas resourceStatePrecision . showChar '\n' .
        showString "Solovay-Kitaev: " . showsWithCommas approxSequences . showChar '\n' .
        (if null phaseShiftFactors then id else showsSKResult) .
        showsWidth width .
        showsTotalGates totalGates .
        showChar '\n'
    showsSKResult =
        showString "Allowed error per Solovay-Kitaev: " . shows errorPerApproxSequence . showChar '\n' .
        showString "Used sequences:\n" .
        flip (foldr showsSQ) phaseShiftFactors
    showsSQ factor =
        showString "  " . shows factor . showString " (" . shows (solutionDistance sol) . showString "): " . showString (solutionLabel sol) . showChar '\n'
      where
        sol = HashMap.lookupDefault (error $ "Cannot find in database: Rotation of 2pi * " ++ show factor) factor sqdbFiltered

buildCircuitSolovayKitaev :: IsCircuit MyGate Wire c => (forall w m. MonadBinaryPhaseShift w m => m ()) -> c
buildCircuitSolovayKitaev circuit =
    buildCircuitToffoli $ runCircuitBuilderPhaseShiftT circuit2
  where
    ~(_, BinaryPhaseShiftStat phaseKickbackDegree maxParallelPhaseKickbacks) =
        runQuantumCounterWithoutMemo (runBinaryPhaseShiftCounterT circuit) 0 ()
    ~(resourceStatePrecision, _approxSequences, _errorPerApproxSequence) =
        chooseResourceStatePrecision phaseKickbackDegree 1e-3
    circuit2 :: forall m. MonadPhaseShift Wire Double m => m ()
    circuit2 = do
        resource <- prepareResourceStates phaseKickbackDegree resourceStatePrecision maxParallelPhaseKickbacks
        runPhaseKickbackT circuit resource

-- The execution of a Haskell program starts with a function called "main".
main :: IO ()
main = do
    sqdbDir <- parseCommandLine
    sqdb <- readSQCTDirectory sqdbDir
    let builder :: (MonadQuantum w m, MonadBinaryPhaseShift w m) => m ()
        builder = do
          xs <- newWires 5
          ys <- newWires 5
          parallel_ (fourierTransform xs) (fourierTransform ys)
    -- Output the cost.
    putStr "\n=== cost ===\n\n"
    putStr $ reportSolovayKitaevCost builder sqdb ""
    -- Generate the circuit, and name the result "circuit".
    let circuit :: IsCircuit MyGate Wire c => c
        circuit = buildCircuitSolovayKitaev builder
    -- Output the circuit with full information (including which part is counted as parallelized).
    putStr "\n=== circuit ===\n\n"
    print (circuit :: Circuit MyGate Wire)
    -- Output the circuit in the QC format.
    putStr "\n=== circuit (QC format) ===\n\n"
    ByteString.putStr $ runQCBuilder IO.nativeNewline circuit
