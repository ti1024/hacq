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
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Quantum.PhaseShift.Binary.Kickback.Estimate (
    module Control.Monad.Quantum.PhaseShift.Binary.Class,
    PhaseKickbackEstimateT(..)) where

import Control.Applicative (Applicative)
import Control.Monad (when, unless)
import Control.Monad.Trans (MonadTrans(lift))
import Data.Functor ((<$>))
import qualified Data.Sequence as Seq

import Control.Monad.Memo.Class
import Control.Monad.Quantum.Class
import Control.Monad.Quantum.PhaseShift.Binary.Class
import Control.Monad.Quantum.ApproxSequence.Class (MonadApproxSequence)
import Control.Monad.Quantum.PhaseShift.Class (MonadPhaseShift)
import Control.Monad.Quantum.Adder (adder, subtractor)
import Control.Monad.Quantum.Counter.Class
import Util ((!))

newtype PhaseKickbackEstimateT c k m a = PhaseKickbackEstimateT {
  -- |Run a `PhaseKickbackEstimateT`, assuming that the resource states have been prepared.
  runPhaseKickbackEstimateT :: m a
} deriving (Functor, Applicative, Monad, MonadQuantumBase w, MonadToffoli w, MonadQuantum w, MonadPhaseShift w angle, MonadApproxSequence w, MonadQuantumCounter w c)

instance MonadTrans (PhaseKickbackEstimateT c k) where
  lift = PhaseKickbackEstimateT

-- The following instance requires UndecidableInstances.
instance MonadMemo (Either k (Int, Integer)) m => MonadMemo k (PhaseKickbackEstimateT c k m) where
  memoize k (PhaseKickbackEstimateT m) = PhaseKickbackEstimateT $
      memoize (Left k) m

instance (MonadQuantum () m, MonadMemo (Either k (Int, Integer)) m) => MonadBinaryPhaseShift () (PhaseKickbackEstimateT c k m) where

  applyGlobalBinaryPhase k _ | k <= 0 = return ()
  applyGlobalBinaryPhase k m = PhaseKickbackEstimateT $ do
      ctrls <- askCtrls
      unless (null ctrls) $
        memoize (Right (k, m)) $
          if m > 0 then
            let ml = integerToBinary m
                low = take (k - 3) ml
                high = take 3 $ drop (k - 3) ml in
            parallels_ [
              when (k >= 1 && length high >= 3 && high !! 2) $
                negateGlobalPhase,
              when (k >= 2 && length high >= 2 && high !! 1) $
                rotateGlobalPhase90 False,
              when (k >= 3 && length high >= 1 && high !! 0) $
                rotateGlobalPhase45 False,
              subtractor (Seq.replicate k ()) (BitConst <$> low) (BitConst False)]
          else
            let ml = integerToBinary (-m)
                low = take (k - 3) ml
                high = take 3 $ drop (k - 3) ml in
            parallels_ [
              when (k >= 1 && length high >= 3 && high !! 2) $
                negateGlobalPhase,
              when (k >= 2 && length high >= 2 && high !! 1) $
                rotateGlobalPhase90 True,
              when (k >= 3 && length high >= 1 && high !! 0) $
                rotateGlobalPhase45 True,
              adder (Seq.replicate k ()) (BitConst <$> low) (BitConst False)]
  {-# INLINABLE applyGlobalBinaryPhase #-}

  applyBinaryPhaseShift k m
    | n <= 0 =
        return ()
    | n == 1 =
        control (m ! 0) $
          applyGlobalBinaryPhase k 1
    | otherwise = compressCtrls 1 $
        parallels_ [
          when (k >= 1 && n >= k) $
            applyZ (m ! (k - 1)),
          when (k >= 2 && n >= k - 1) $
            applyS (m ! (k - 2)) False,
          when (k >= 3 && n >= k - 2) $
            applyT (m ! (k - 3)) False,
          when (k >= 4) $
            subtractor (Seq.replicate k ()) (Seq.take (k - 3) m) (BitConst False)]
    where
      n = min k (Seq.length m)
  {-# INLINABLE applyBinaryPhaseShift #-}

integerToBinary :: Integer -> [Bool]
integerToBinary 0 = []
integerToBinary m = odd m : integerToBinary (m `div` 2)
