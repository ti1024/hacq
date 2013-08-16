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

module Control.Monad.Quantum.PhaseShift.Null (module Control.Monad.Quantum.PhaseShift.Class,
    PhaseShiftNullT(..)) where

import Control.Applicative (Applicative)
import Control.Monad.Trans (MonadTrans, lift)

import Control.Monad.Memo.Class
import Control.Monad.Quantum.Counter
import Control.Monad.Quantum.PhaseShift.Binary.Class
import Control.Monad.Quantum.PhaseShift.Class

newtype PhaseShiftNullT w angle m a = PhaseShiftNullT {
  runPhaseShiftNullT :: m a
} deriving (Functor, Applicative, Monad, MonadQuantumBase w, MonadToffoli w, MonadQuantum w, MonadBinaryPhaseShift w, MonadQuantumCounter w c, MonadMemo k)

instance MonadTrans (PhaseShiftNullT angle w) where
  lift = PhaseShiftNullT

instance (MonadQuantum w m, RealFrac angle) => MonadPhaseShift w angle (PhaseShiftNullT w angle m) where
  applyGlobalPhase _fraction = return ()
