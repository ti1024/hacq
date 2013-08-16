{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Monad.Quantum.Base.Counter (module Control.Monad.Quantum.Base.Class, module Control.Monad.Quantum.Counter.Class,
    QuantumCounterBase, runQuantumCounterBase, execQuantumCounterBase, runQuantumCounterBaseWithoutMemo) where

import Control.Applicative (Applicative)
import Control.Arrow (second)
import Control.Monad (when)
import Control.Monad.Writer.Strict (WriterT(WriterT), runWriterT, execWriterT)
import qualified Control.Monad.Writer.Strict as Writer
import Data.Functor ((<$>))
import Data.Hashable (Hashable)
import qualified Data.Sequence as Seq
import Data.Typeable (Typeable)

import Control.Monad.Ask.Class (MonadAsk)
import Control.Monad.Memo
import Control.Monad.Quantum.Base.Class
import Control.Monad.Quantum.Counter.Class
import Data.Quantum.CountKey
import Data.Quantum.Cost.Class (Cost, WrappedCost(WrappedCost), unwrapCost)
import qualified Data.Quantum.Cost.Class as Cost
import Data.Void

newtype QuantumCounterBase s r c k a = QuantumCounterBase {
  unwrapQuantumCounterBase :: WriterT (WrappedCost c) (Memo s r k) a
} deriving Functor

deriving instance Cost c => Applicative (QuantumCounterBase s r c k)
deriving instance Cost c => Monad (QuantumCounterBase s r c k)
deriving instance Cost c => MonadAsk r (QuantumCounterBase s r c k)

recordCounter :: Cost c => c -> QuantumCounterBase s r c k ()
recordCounter c = QuantumCounterBase $ Writer.tell $ WrappedCost c
{-# INLINABLE recordCounter #-}

instance Cost c => MonadQuantumBase () (QuantumCounterBase s r c k) where

  newWire = do
      recordCounter Cost.newWire
      return ()
  {-# INLINABLE newWire #-}

  ancilla = do
      recordCounter Cost.newWire
      return ()
  {-# INLINABLE ancilla #-}

  applyX _w =
      return ()
  {-# INLINABLE applyX #-}

  applyY _w =
      return ()
  {-# INLINABLE applyY #-}

  rawApplyZ _w _neg =
      return ()
  {-# INLINABLE rawApplyZ #-}

  applyH _w =
      recordCounter $ Cost.gate KGateH
  {-# INLINABLE applyH #-}

  rawApplyS _w _neg _inv =
      recordCounter $ Cost.gate KGateS
  {-# INLINABLE rawApplyS #-}

  rawApplyT _w _neg _inv =
      recordCounter $ Cost.gate KGateT
  {-# INLINABLE rawApplyT #-}

  negateGlobalPhase =
      return ()
  {-# INLINABLE negateGlobalPhase #-}

  rotateGlobalPhase90 _inv =
      return ()
  {-# INLINABLE rotateGlobalPhase90 #-}

  rotateGlobalPhase45 _inv =
      return ()
  {-# INLINABLE rotateGlobalPhase45 #-}

  rawApplyZC a nega b negb =
      with_ (do
          when nega $ applyX a
          applyH a) $
        rawCnotWire a b negb
  {-# INLINABLE rawApplyZC #-}

  rawCnotWire _w _w1 _neg =
      recordCounter $ Cost.gate KGateXC
  {-# INLINABLE rawCnotWire #-}

  rawCnotWires ws =
      mapParM_ (\(w, w1, neg) -> rawCnotWire w w1 neg) ws
  {-# INLINABLE rawCnotWires #-}

  with prepare body = QuantumCounterBase $ WriterT $ do
      (a, WrappedCost prepareLog) <- runWriterT $ unwrapQuantumCounterBase prepare
      (b, WrappedCost bodyLog) <- runWriterT $ unwrapQuantumCounterBase (body a)
      return (b, WrappedCost $ Cost.with prepareLog bodyLog)
  {-# INLINABLE with #-}

  bindParallel m1 m2 = QuantumCounterBase $ WriterT $ do
      (a1, WrappedCost l1) <- runWriterT $ unwrapQuantumCounterBase m1
      (a2, WrappedCost l2) <- runWriterT $ unwrapQuantumCounterBase (m2 a1)
      return (a2, WrappedCost $ Cost.parallel l1 l2)
  {-# INLINABLE bindParallel #-}

  invert m = QuantumCounterBase $
      Writer.censor (WrappedCost . Cost.adjoint . unwrapCost) $ unwrapQuantumCounterBase m
  {-# INLINABLE invert #-}

  withoutCtrls m = m

  replicateQ n m = QuantumCounterBase $
      Seq.replicate n <$> Writer.censor (WrappedCost . Cost.replicateSequential n . unwrapCost) (unwrapQuantumCounterBase m)
  {-# INLINABLE replicateQ #-}

  replicateParallelQ n m = QuantumCounterBase $
      Seq.replicate n <$> Writer.censor (WrappedCost . Cost.replicateParallel n . unwrapCost) (unwrapQuantumCounterBase m)
  {-# INLINABLE replicateParallelQ #-}

  replicateQ_ n m = QuantumCounterBase $
      Writer.censor (WrappedCost . Cost.replicateSequential n . unwrapCost) $ unwrapQuantumCounterBase m
  {-# INLINABLE replicateQ_ #-}

  replicateParallelQ_ n m = QuantumCounterBase $
      Writer.censor (WrappedCost . Cost.replicateParallel n . unwrapCost) $ unwrapQuantumCounterBase m
  {-# INLINABLE replicateParallelQ_ #-}

  genericReplicateQ_ n m = QuantumCounterBase $
      Writer.censor (WrappedCost . Cost.genericReplicateSequential n . unwrapCost) $ unwrapQuantumCounterBase m
  {-# INLINABLE genericReplicateQ_ #-}

  genericReplicateParallelQ_ n m = QuantumCounterBase $
      Writer.censor (WrappedCost . Cost.genericReplicateParallel n . unwrapCost) $ unwrapQuantumCounterBase m
  {-# INLINABLE genericReplicateParallelQ_ #-}

instance Cost c => MonadQuantumCounter () c (QuantumCounterBase s r c k) where
  rawRecord = recordCounter

instance (Cost c, Typeable c, Eq k, Hashable k) => MonadMemo k (QuantumCounterBase s r c k) where
  memoize k m = QuantumCounterBase $ WriterT $ second WrappedCost <$> memoize k (second unwrapCost <$> runWriterT (unwrapQuantumCounterBase m))

runQuantumCounterBase :: (forall s. QuantumCounterBase s r c k a) -> r -> (a, c)
runQuantumCounterBase m r =
    (a, c)
  where
    (a, WrappedCost c) = runMemo (runWriterT (unwrapQuantumCounterBase m)) r

execQuantumCounterBase :: (forall s. QuantumCounterBase s r c k a) -> r -> c
execQuantumCounterBase m =
    unwrapCost . runMemo (execWriterT (unwrapQuantumCounterBase m))

runQuantumCounterBaseWithoutMemo :: (forall s. QuantumCounterBase s r c Void a) -> r -> (a, c)
runQuantumCounterBaseWithoutMemo = runQuantumCounterBase
