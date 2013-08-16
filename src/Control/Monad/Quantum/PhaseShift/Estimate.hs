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

module Control.Monad.Quantum.PhaseShift.Estimate (module Control.Monad.Quantum.PhaseShift.Class,
    PhaseShiftEstimateT, runPhaseShiftEstimateT) where

import Control.Applicative (Applicative)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)

import Control.Monad.Memo.Class
import Control.Monad.Quantum.Counter
import Control.Monad.Quantum.PhaseShift.Class
import Control.Monad.Quantum.PhaseShift.Binary.Class

newtype PhaseShiftEstimateT w angle m a = PhaseShiftEstimateT {
  unwrapPhaseShiftEstimateT :: ReaderT Int m a
} deriving (Functor, Applicative, Monad, MonadQuantumBase w, MonadToffoli w, MonadQuantum w, MonadBinaryPhaseShift w, MonadQuantumCounter w c)

deriving instance MonadMemo (k, Int) m => MonadMemo k (PhaseShiftEstimateT w angle m)

instance MonadTrans (PhaseShiftEstimateT angle w) where
  lift = PhaseShiftEstimateT . lift

instance (MonadBinaryPhaseShift w m, RealFrac angle) => MonadPhaseShift w angle (PhaseShiftEstimateT w angle m) where
  applyGlobalPhase fraction = PhaseShiftEstimateT $ ReaderT $ \binaryPhaseShiftPrec ->
      applyGlobalBinaryPhase binaryPhaseShiftPrec (roundAngle binaryPhaseShiftPrec fraction)
  {-# INLINABLE applyGlobalPhase #-}

roundAngle :: RealFrac angle => Int -> angle -> Integer
roundAngle binaryPhaseShiftPrec fraction =
    if binaryPhaseShiftPrec > 0 then
      round (fraction * 2 ^ binaryPhaseShiftPrec)
    else
      0

runPhaseShiftEstimateT :: PhaseShiftEstimateT w angle m a -> Int -> m a
runPhaseShiftEstimateT m binaryPhaseShiftPrec =
    runReaderT (unwrapPhaseShiftEstimateT m) binaryPhaseShiftPrec
