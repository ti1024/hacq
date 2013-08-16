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

module Control.Monad.Quantum.ApproxSequence.Estimate (
    ApproxSequenceEstimateT, runApproxSequenceEstimateT, approxSequenceLength) where

import Control.Applicative (Applicative)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)
import Control.Monad.Trans (MonadTrans(lift))

import Control.Monad.Memo.Class
import Control.Monad.Quantum.Class
import Control.Monad.Quantum.ApproxSequence.Class
import Control.Monad.Quantum.Counter.Class

-- |Estimation of the length of the Solovay-Kitaev sequence based on Kliuchnikov's formula.
newtype ApproxSequenceEstimateT w m a = ApproxSequenceEstimateT {
  unwrapApproxSequenceEstimateT :: ReaderT Integer m a
} deriving (Functor, Applicative, Monad, MonadQuantumBase w, MonadToffoli w, MonadQuantum w, MonadQuantumCounter w c)

deriving instance MonadMemo (k, Integer) m => MonadMemo k (ApproxSequenceEstimateT w m)

instance MonadTrans (ApproxSequenceEstimateT w) where
  lift = ApproxSequenceEstimateT . lift

instance MonadQuantumBase w m => MonadApproxSequence w (ApproxSequenceEstimateT w m) where
  applyOneQubitUnitary _ _ _ w = ApproxSequenceEstimateT $ ReaderT $ \seqLength ->
      genericReplicateQ_ seqLength $ applyT (bit w) False
  {-# INLINABLE applyOneQubitUnitary #-}

runApproxSequenceEstimateT :: ApproxSequenceEstimateT w m a -> Integer -> m a
runApproxSequenceEstimateT = runReaderT . unwrapApproxSequenceEstimateT
{-# INLINABLE runApproxSequenceEstimateT #-}

-- |Compute the length of the Solovay-Kitaev sequence based on Kliuchnikov's formula,
-- assuming that each one-qubit unitary U must be implemented as U'
-- such that the diamond norm (completely bounded 1-norm) of the diffefence of the two unitary channels
-- is at most eps.
approxSequenceLength :: Double -> Integer
approxSequenceLength eps =
    ceiling $ 40.5774 * ((-3.465735903 + log (2 / eps)) ** 3.969362296)
