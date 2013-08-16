{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Quantum.PhaseShift.CircuitBuilder (
    module Control.Monad.Quantum.PhaseShift.CircuitBuilder,
    CircuitBuilderPhaseShiftT(..)) where

import Control.Applicative (Applicative)
import Control.Monad.Trans (MonadTrans(lift))
import qualified Data.Set as Set

import Data.Quantum.Circuit (Wire(Wire), IsCircuit)
import qualified Data.Quantum.Circuit as Circuit
import Data.Quantum.PhaseShift.GatePhaseShift
import Control.Monad.Quantum.CircuitBuilder.Class
import Control.Monad.Quantum.PhaseShift.Class

-- | @CircuitBuilder@ is a monad which maintains the current control context, the next wire index,
-- and the sequence of elementary gates.
newtype CircuitBuilderPhaseShiftT m a = CircuitBuilderPhaseShiftT {
  runCircuitBuilderPhaseShiftT :: m a
} deriving (Functor, Applicative, Monad, MonadQuantumBase w, MonadToffoli w, MonadQuantum w)

instance MonadTrans CircuitBuilderPhaseShiftT where
  lift = CircuitBuilderPhaseShiftT

deriving instance MonadCircuitBuilder c m => MonadCircuitBuilder c (CircuitBuilderPhaseShiftT m)

instance (IsCircuit g Wire c, IsGatePhaseShift g, MonadQuantum Wire m, MonadCircuitBuilder c m) => MonadPhaseShift Wire Double (CircuitBuilderPhaseShiftT m) where
  applyGlobalPhase fraction = CircuitBuilderPhaseShiftT $
      handleMaybeCtrl $ \ctrl ->
        case ctrl of
          Nothing -> return ()
          Just c@(Wire ci) ->
            recordGate (Circuit.singleton $ gatePhaseShift c fraction) (Set.singleton ci)
