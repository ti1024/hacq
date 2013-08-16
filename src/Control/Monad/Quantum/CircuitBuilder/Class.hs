{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Quantum.CircuitBuilder.Class (MonadCircuitBuilder(..)) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (lift)
import Data.Set (Set)

import Control.Monad.Quantum.Base.Class
import Data.Quantum.Circuit.Class

class MonadQuantumBase Wire m => MonadCircuitBuilder c m | m -> c where
  recordGate :: c -> Set Integer -> m ()

-- Instance for ReaderT
-- This instance requires UndecidableInstances.

instance MonadCircuitBuilder c m => MonadCircuitBuilder c (ReaderT r m) where
  recordGate c touched = lift $ recordGate c touched
  {-# INLINABLE recordGate #-}
