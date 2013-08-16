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

module Control.Monad.Quantum.ApproxSequence.Null (
    module Control.Monad.Quantum.ApproxSequence.Class,
    ApproxSequenceNullT(..)) where

import Control.Applicative (Applicative)
import Control.Monad.Trans (MonadTrans(lift))

import Control.Monad.Memo.Class
import Control.Monad.Quantum.Class
import Control.Monad.Quantum.Counter.Class
import Control.Monad.Quantum.ApproxSequence.Class

newtype ApproxSequenceNullT w m a = ApproxSequenceNullT {
  runApproxSequenceNullT :: m a
} deriving (Functor, Applicative, Monad, MonadQuantumBase w, MonadToffoli w, MonadQuantum w, MonadQuantumCounter w c, MonadMemo k)

instance MonadTrans (ApproxSequenceNullT w) where
  lift = ApproxSequenceNullT

instance MonadQuantum w m => MonadApproxSequence w (ApproxSequenceNullT w m) where
  applyOneQubitUnitary _ _ _ _ =
      return ()
