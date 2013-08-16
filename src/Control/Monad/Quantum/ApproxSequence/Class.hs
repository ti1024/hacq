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

module Control.Monad.Quantum.ApproxSequence.Class (MonadApproxSequence(..), applyPrepareQubitApprox, applyGlobalPhase) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Complex

import Control.Monad.Quantum.Class

class MonadQuantumBase w m => MonadApproxSequence w m | m -> w where
  -- |@applyOneQubitUnitary a c d w@ applies unitary U to wire w, where U is given by the following matrix:
  --
  -- > a c*
  -- > c d
  applyOneQubitUnitary :: Complex Double -> Complex Double -> Complex Double -> w -> m ()

-- |@applyPrepareQubitApprox a b@ prepares a qubit in a state a|0>+b|1>.
applyPrepareQubitApprox :: MonadApproxSequence w m => Complex Double -> Complex Double -> m w
applyPrepareQubitApprox a b = do
    w <- ancilla
    applyOneQubitUnitary a b (-conjugate a) w
    return w
{-# INLINABLE applyPrepareQubitApprox #-}

applyGlobalPhase :: (MonadQuantum w m, MonadApproxSequence w m) => Double -> m ()
applyGlobalPhase fraction =
    handleMaybeCtrl $ \ctrl ->
      case ctrl of
        Nothing -> return ()
        Just ctrlwire ->
          applyOneQubitUnitary 1 0 (cis (2 * pi * fraction)) ctrlwire
{-# INLINABLE applyGlobalPhase #-}

-- Instance for ReaderT

instance MonadApproxSequence w m => MonadApproxSequence w (ReaderT r m) where
  applyOneQubitUnitary a c d w = lift $ applyOneQubitUnitary a c d w
  {-# INLINABLE applyOneQubitUnitary #-}
