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

module Control.Monad.Quantum.PhaseShift.Counter (module Control.Monad.Quantum.PhaseShift.Class,
    PhaseShiftCounterT(..)) where

import Control.Applicative (Applicative)
import Control.Monad.Trans (MonadTrans, lift)

import Control.Monad.Memo.Class
import Control.Monad.Quantum.Counter
import Control.Monad.Quantum.PhaseShift.Class
import Data.Quantum.PhaseShift.Count

newtype PhaseShiftCounterT w c angle m a = PhaseShiftCounterT {
  runPhaseShiftCounterT :: m a
} deriving (Functor, Applicative, Monad, MonadQuantumBase w, MonadToffoli w, MonadQuantum w, MonadQuantumCounter w c, MonadMemo k)

instance MonadTrans (PhaseShiftCounterT w c angle) where
  lift = PhaseShiftCounterT

instance (IsPhaseShiftCount c, MonadPhaseShift w angle m, MonadQuantumCounter w c m) => MonadPhaseShift w angle (PhaseShiftCounterT w c angle m) where
  applyGlobalPhase fraction = do
      rawRecord singletonPhaseShiftCount
      lift $ applyGlobalPhase fraction
