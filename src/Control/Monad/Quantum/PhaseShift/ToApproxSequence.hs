{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Quantum.PhaseShift.ToApproxSequence (module Control.Monad.Quantum.PhaseShift.Class,
    PhaseShiftToApproxSequenceT(..)) where

import Control.Applicative (Applicative)
import Control.Monad.Trans (MonadTrans(lift))

import Control.Monad.Memo.Class
import Control.Monad.Quantum.Counter
import Control.Monad.Quantum.PhaseShift.Class (MonadPhaseShift)
import qualified Control.Monad.Quantum.PhaseShift.Class as PhaseShift
import Control.Monad.Quantum.ApproxSequence.Class (MonadApproxSequence)
import qualified Control.Monad.Quantum.ApproxSequence.Class as ApproxSequence

newtype PhaseShiftToApproxSequenceT w m a = PhaseShiftToApproxSequenceT {
  runPhaseShiftToApproxSequenceT :: m a
} deriving (Functor, Applicative, Monad, MonadQuantumBase w, MonadToffoli w, MonadQuantum w, MonadApproxSequence w, MonadQuantumCounter w c, MonadMemo k)

instance MonadTrans (PhaseShiftToApproxSequenceT w) where
  lift = PhaseShiftToApproxSequenceT

instance (MonadQuantum w m, MonadApproxSequence w m) => MonadPhaseShift w Double (PhaseShiftToApproxSequenceT w m) where
  applyGlobalPhase fraction = PhaseShiftToApproxSequenceT $
      ApproxSequence.applyGlobalPhase fraction
  {-# INLINABLE applyGlobalPhase #-}
