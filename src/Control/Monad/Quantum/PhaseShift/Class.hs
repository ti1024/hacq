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

module Control.Monad.Quantum.PhaseShift.Class (module Control.Monad.Quantum.Class,
    MonadPhaseShift(..), applyPhaseShift) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (MonadTrans, lift)

import Control.Monad.Quantum.Class

class (MonadQuantum w m, RealFrac angle) => MonadPhaseShift w angle m | m -> w, m -> angle where
  applyGlobalPhase :: angle -> m ()

applyPhaseShift :: MonadPhaseShift w angle m => angle -> Bit w -> m ()
applyPhaseShift fraction w =
    control w $ applyGlobalPhase fraction

instance MonadPhaseShift w angle m => MonadPhaseShift w angle (ReaderT r m) where
  applyGlobalPhase fraction = lift $ applyGlobalPhase fraction
  {-# INLINABLE applyGlobalPhase #-}
