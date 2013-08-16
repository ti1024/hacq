{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.Quantum.CircuitBuilder (
    CircuitBuilder, runCircuitBuilder, evalCircuitBuilder, evalCircuitBuilder_, execCircuitBuilder,
    buildCircuit) where

import Control.Monad.Quantum.Base.CircuitBuilder
import Control.Monad.Quantum.Toffoli.ToStandard
import Control.Monad.Quantum.Control
import Data.Quantum.Circuit.Class (Wire, IsCircuit)
import Data.Quantum.Circuit.Invert (CircuitInvert)
import Data.Quantum.Circuit.MinimizeWireIndices

type CircuitBuilder c = QuantumControlT Wire (ToffoliToStandardT Wire (CircuitBuilderBase c))

runCircuitBuilder :: CircuitBuilder c a -> [Wire] -> Integer -> (a, Integer, c)
runCircuitBuilder m ctrls nextWireIndex =
    runCircuitBuilderBase (runToffoliToStandardT $ runQuantumControlT m ctrls) nextWireIndex

evalCircuitBuilder :: IsCircuit g Wire c => CircuitBuilder c a -> [Wire] -> Integer -> (a, c)
evalCircuitBuilder m ctrls nextWireIndex =
    evalCircuitBuilderBase (runToffoliToStandardT $ runQuantumControlT m ctrls) nextWireIndex

evalCircuitBuilder_ :: IsCircuit g Wire c => CircuitBuilder c a -> [Wire] -> Integer -> c
evalCircuitBuilder_ m ctrls nextWireIndex =
    evalCircuitBuilderBase_ (runToffoliToStandardT $ runQuantumControlT m ctrls) nextWireIndex

execCircuitBuilder :: IsCircuit g Wire c => CircuitBuilder c a -> [Wire] -> Integer -> (Integer, c)
execCircuitBuilder m ctrls nextWireIndex =
    execCircuitBuilderBase (runToffoliToStandardT $ runQuantumControlT m ctrls) nextWireIndex

buildCircuit :: IsCircuit g Wire c => CircuitBuilder (CircuitInvert (MinimizeWireIndices c)) a -> c
buildCircuit m =
    buildCircuitBase $ runToffoliToStandardT $ runQuantumControlT m []
