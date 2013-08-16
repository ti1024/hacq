{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.Quantum.CircuitBuilderToffoli (
    CircuitBuilderToffoli, runCircuitBuilderToffoli, evalCircuitBuilderToffoli, evalCircuitBuilderToffoli_, execCircuitBuilderToffoli,
    buildCircuitToffoli) where

import Control.Monad.Quantum.Base.CircuitBuilder
import Control.Monad.Quantum.Control
import Data.Quantum.Circuit.Class (Wire, IsCircuit)
import Data.Quantum.Circuit.Invert (CircuitInvert)
import Data.Quantum.Circuit.MinimizeWireIndices

type CircuitBuilderToffoli c = QuantumControlT Wire (CircuitBuilderBase c)

runCircuitBuilderToffoli :: CircuitBuilderToffoli c a -> [Wire] -> Integer -> (a, Integer, c)
runCircuitBuilderToffoli m ctrls nextWireIndex =
    runCircuitBuilderBase (runQuantumControlT m ctrls) nextWireIndex

evalCircuitBuilderToffoli :: IsCircuit g Wire c => CircuitBuilderToffoli c a -> [Wire] -> Integer -> (a, c)
evalCircuitBuilderToffoli m ctrls nextWireIndex =
    evalCircuitBuilderBase (runQuantumControlT m ctrls) nextWireIndex

evalCircuitBuilderToffoli_ :: IsCircuit g Wire c => CircuitBuilderToffoli c a -> [Wire] -> Integer -> c
evalCircuitBuilderToffoli_ m ctrls nextWireIndex =
    evalCircuitBuilderBase_ (runQuantumControlT m ctrls) nextWireIndex

execCircuitBuilderToffoli :: IsCircuit g Wire c => CircuitBuilderToffoli c a -> [Wire] -> Integer -> (Integer, c)
execCircuitBuilderToffoli m ctrls nextWireIndex =
    execCircuitBuilderBase (runQuantumControlT m ctrls) nextWireIndex

buildCircuitToffoli :: IsCircuit g Wire c => CircuitBuilderToffoli (CircuitInvert (MinimizeWireIndices c)) a -> c
buildCircuitToffoli m =
    buildCircuitBase $ runQuantumControlT m []
