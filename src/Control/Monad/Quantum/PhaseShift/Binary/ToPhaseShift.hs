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

module Control.Monad.Quantum.PhaseShift.Binary.ToPhaseShift (
    module Control.Monad.Quantum.PhaseShift.Binary.Class,
    BinaryPhaseShiftToPhaseShiftT(..)) where

import Control.Applicative (Applicative)
import Control.Monad.Trans (MonadTrans(lift))

import Control.Monad.Memo.Class
import Control.Monad.Quantum.Class
import Control.Monad.Quantum.Counter.Class
import Control.Monad.Quantum.PhaseShift.Binary.Class
import Control.Monad.Quantum.ApproxSequence.Class (MonadApproxSequence)
import Control.Monad.Quantum.PhaseShift.Class (MonadPhaseShift, applyGlobalPhase)

newtype BinaryPhaseShiftToPhaseShiftT w angle m a = BinaryPhaseShiftToPhaseShiftT {
  runPhaseKickbackConvertToPhaseShiftT :: m a
} deriving (Functor, Applicative, Monad, MonadQuantumBase w, MonadToffoli w, MonadQuantum w, MonadQuantumCounter w c, MonadMemo k, MonadApproxSequence w, MonadPhaseShift w angle)

instance MonadTrans (BinaryPhaseShiftToPhaseShiftT w angle) where
  lift = BinaryPhaseShiftToPhaseShiftT

instance MonadPhaseShift w angle m => MonadBinaryPhaseShift w (BinaryPhaseShiftToPhaseShiftT w angle m) where
  applyGlobalBinaryPhase k m = BinaryPhaseShiftToPhaseShiftT $
      applyGlobalPhase (fromInteger m / 2 ^ k)
  applyBinaryPhaseShift _ _ =
      error "BinaryPhaseShiftToPhaseShiftT: applyBinaryPhaseShift is not supported"
