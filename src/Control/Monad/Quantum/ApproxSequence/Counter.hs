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

module Control.Monad.Quantum.ApproxSequence.Counter (
    module Control.Monad.Quantum.ApproxSequence.Class,
    ApproxSequenceCounterT(..)) where

import Control.Applicative (Applicative)
import Control.Monad.Trans (MonadTrans(lift))

import Control.Monad.Memo.Class
import Control.Monad.Quantum.Class
import Control.Monad.Quantum.Counter.Class
import Control.Monad.Quantum.ApproxSequence.Class
import Data.Quantum.ApproxSequence.Count

newtype ApproxSequenceCounterT w c m a = ApproxSequenceCounterT {
  runApproxSequenceCounterT :: m a
} deriving (Functor, Applicative, Monad, MonadQuantumBase w, MonadToffoli w, MonadQuantum w, MonadQuantumCounter w c, MonadMemo k)

instance MonadTrans (ApproxSequenceCounterT w c) where
  lift = ApproxSequenceCounterT

instance (IsApproxSequenceCount c, MonadApproxSequence w m, MonadQuantumCounter w c m) => MonadApproxSequence w (ApproxSequenceCounterT w c m) where
  applyOneQubitUnitary a c d w = do
      rawRecord singletonApproxSequenceCount
      lift $ applyOneQubitUnitary a c d w
