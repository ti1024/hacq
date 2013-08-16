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

module Control.Monad.Quantum.PhaseShift.SQCT (module Control.Monad.Quantum.PhaseShift.Class,
    PhaseShiftSQCTT(..)) where

import Control.Applicative (Applicative)
import Control.Monad.Trans (MonadTrans(lift))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Control.Monad.Ask.Class (MonadAsk, asks)
import Control.Monad.Memo.Class
import Control.Monad.Quantum.Counter
import Control.Monad.Quantum.PhaseShift.Class
import Control.Monad.Quantum.PhaseShift.Binary.Class
import Control.Monad.Quantum.SQCT
import Data.Quantum.SQCT

class ToSQDatabase r where
  toSQDatabase :: r -> HashMap Rational SQSolution

instance ToSQDatabase (HashMap Rational SQSolution) where
  toSQDatabase = id

newtype PhaseShiftSQCTT w m a = PhaseShiftSQCTT {
  runPhaseShiftSQCTT :: m a
} deriving (Functor, Applicative, Monad, MonadAsk r, MonadMemo k, MonadQuantumBase w, MonadToffoli w, MonadQuantum w, MonadBinaryPhaseShift w, MonadQuantumCounter w c)

instance MonadTrans (PhaseShiftSQCTT w) where
  lift = PhaseShiftSQCTT

instance (MonadQuantum w m, MonadAsk r m, ToSQDatabase r) => MonadPhaseShift w Rational (PhaseShiftSQCTT w m) where
  applyGlobalPhase fraction = PhaseShiftSQCTT $
      handleMaybeCtrl $ \ctrl -> case ctrl of
        Nothing ->
          return ()
        Just c -> do
          sqdb <- asks toSQDatabase
          case HashMap.lookup fraction sqdb of
            Nothing ->
              error $ "PhaseShiftSQCTT: Unknown angle: 2pi * " ++ show fraction
            Just sol ->
              applySQGateList (solutionGateList sol) c
  {-# INLINABLE applyGlobalPhase #-}
