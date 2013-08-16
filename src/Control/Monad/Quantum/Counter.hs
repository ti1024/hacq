{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE RankNTypes #-}

module Control.Monad.Quantum.Counter (module Control.Monad.Quantum.Class, module Control.Monad.Quantum.Counter.Class,
    QuantumCounter, runQuantumCounter, execQuantumCounter, runQuantumCounterWithoutMemo, generateCost) where

import Control.Monad.Quantum.Class
import Control.Monad.Quantum.Base.Counter
import Control.Monad.Quantum.Counter.Class
import Control.Monad.Quantum.Toffoli.ToStandard
import Control.Monad.Quantum.Control
import Data.Void

type QuantumCounter s r c k = QuantumControlT () (ToffoliToStandardT () (QuantumCounterBase s r c (k, [()])))

runQuantumCounter :: (forall s. QuantumCounter s r c k a) -> Int -> r -> (a, c)
runQuantumCounter m ctrls =
    runQuantumCounterBase $ runToffoliToStandardT $ runQuantumControlT m $ replicate ctrls ()

execQuantumCounter :: (forall s. QuantumCounter s r c k a) -> Int -> r -> c
execQuantumCounter m ctrls =
    execQuantumCounterBase $ runToffoliToStandardT $ runQuantumControlT m $ replicate ctrls ()

runQuantumCounterWithoutMemo :: (forall s. QuantumCounter s r c Void a) -> Int -> r -> (a, c)
runQuantumCounterWithoutMemo = runQuantumCounter

generateCost :: (forall s. QuantumCounter s r c k a) -> r -> c
generateCost m =
    execQuantumCounter m 0
