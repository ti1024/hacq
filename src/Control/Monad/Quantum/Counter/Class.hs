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

module Control.Monad.Quantum.Counter.Class (MonadQuantumCounter(..)) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (lift)

import Control.Monad.Quantum.Base.Class

class MonadQuantumBase w m => MonadQuantumCounter w c m | m -> w, m -> c where
  rawRecord :: c -> m ()

-- Instance for ReaderT
-- This instance requires UndecidableInstances.

instance MonadQuantumCounter w c m => MonadQuantumCounter w c (ReaderT r m) where
  rawRecord = lift . rawRecord
  {-# INLINABLE rawRecord #-}
