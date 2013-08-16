{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Quantum.PhaseShift.Binary.Class (module Control.Monad.Quantum.Class,
    MonadBinaryPhaseShift(..), applyBinaryPhaseShiftConst) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Sequence (Seq)

import Control.Monad.Memo.Null
import Control.Monad.Quantum.Class

class MonadQuantum w m => MonadBinaryPhaseShift w m | m -> w where

  applyGlobalBinaryPhase :: Int -> Integer -> m ()

  -- |@applyBinaryPhaseShift k m@ maps |m> to e^{2πim/2^k}|m>.
  --
  -- The first qubit in @m@ is LSB, and @m@ is considered as unsigned.
  applyBinaryPhaseShift :: Int -> Seq (Bit w) -> m ()

-- |@applyBinaryPhaseShiftConst k m w@ maps |0> to |0> and |1> to e^{2πim/2^k}|1>.
applyBinaryPhaseShiftConst :: MonadBinaryPhaseShift w m => Int -> Integer -> Bit w -> m ()
applyBinaryPhaseShiftConst k m w =
    control w $ applyGlobalBinaryPhase k m

-- The following instance requires UndecidableInstances.

instance MonadBinaryPhaseShift w m => MonadBinaryPhaseShift w (ReaderT r m) where
  applyGlobalBinaryPhase k m =
      lift $ applyGlobalBinaryPhase k m
  {-# INLINABLE applyGlobalBinaryPhase #-}
  applyBinaryPhaseShift k m =
      lift $ applyBinaryPhaseShift k m
  {-# INLINABLE applyBinaryPhaseShift #-}

-- The following instance requires UndecidableInstances.

instance MonadBinaryPhaseShift w m => MonadBinaryPhaseShift w (MemoNullT k m) where
  applyGlobalBinaryPhase k m =
      lift $ applyGlobalBinaryPhase k m
  {-# INLINABLE applyGlobalBinaryPhase #-}
  applyBinaryPhaseShift k m =
      lift $ applyBinaryPhaseShift k m
  {-# INLINABLE applyBinaryPhaseShift #-}
