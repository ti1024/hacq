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

module Control.Monad.Quantum.PhaseShift.Binary.Counter (
    module Control.Monad.Quantum.PhaseShift.Binary.Class,
    BinaryPhaseShiftCounterT(..)) where

import Control.Applicative (Applicative)
import Control.Monad.Trans (MonadTrans(lift))

import Control.Monad.Memo.Class
import Control.Monad.Quantum.Class
import Control.Monad.Quantum.Counter.Class
import Control.Monad.Quantum.PhaseShift.Binary.Class
import Control.Monad.Quantum.ApproxSequence.Class
import Data.Quantum.PhaseShift.Binary.Stat

newtype BinaryPhaseShiftCounterT w c m a = BinaryPhaseShiftCounterT {
  runBinaryPhaseShiftCounterT :: m a
} deriving (Functor, Applicative, Monad, MonadQuantumBase w, MonadToffoli w, MonadQuantum w, MonadQuantumCounter w c, MonadMemo k, MonadApproxSequence w)

instance MonadTrans (BinaryPhaseShiftCounterT w c) where
  lift = BinaryPhaseShiftCounterT

instance (IsBinaryPhaseShiftStat c, MonadQuantum w m, MonadQuantumCounter w c m) => MonadBinaryPhaseShift w (BinaryPhaseShiftCounterT w c m) where
  applyGlobalBinaryPhase k _
    | k <= 3 =
        return ()
    | otherwise =
        rawRecord $ singletonBinaryPhaseShiftStat k
  applyBinaryPhaseShift k _
    | k <= 3 =
        return ()
    | otherwise =
        rawRecord $ singletonBinaryPhaseShiftStat k
