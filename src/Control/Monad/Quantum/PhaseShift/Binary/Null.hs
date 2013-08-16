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

module Control.Monad.Quantum.PhaseShift.Binary.Null (
    module Control.Monad.Quantum.PhaseShift.Binary.Class,
    BinaryPhaseShiftNullT(..)) where

import Control.Applicative (Applicative)
import Control.Monad.Trans (MonadTrans(lift))

import Control.Monad.Memo.Class
import Control.Monad.Quantum.Class
import Control.Monad.Quantum.Counter.Class
import Control.Monad.Quantum.PhaseShift.Binary.Class
import Control.Monad.Quantum.ApproxSequence.Class (MonadApproxSequence)
import Control.Monad.Quantum.PhaseShift.Class (MonadPhaseShift)

newtype BinaryPhaseShiftNullT w m a = BinaryPhaseShiftNullT {
  runBinaryPhaseShiftNullT :: m a
} deriving (Functor, Applicative, Monad, MonadQuantumBase w, MonadToffoli w, MonadQuantum w, MonadQuantumCounter w c, MonadMemo k, MonadApproxSequence w, MonadPhaseShift w angle)

instance MonadTrans (BinaryPhaseShiftNullT w) where
  lift = BinaryPhaseShiftNullT

instance MonadQuantum w m => MonadBinaryPhaseShift w (BinaryPhaseShiftNullT w m) where
  applyGlobalBinaryPhase _ _ =
      return ()
  {-# INLINABLE applyGlobalBinaryPhase #-}
  applyBinaryPhaseShift _ _ =
      return ()
  {-# INLINABLE applyBinaryPhaseShift #-}
