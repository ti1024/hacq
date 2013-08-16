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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Quantum.Control (QuantumControlT, runQuantumControlT) where

import Control.Applicative (Applicative)
import Control.Monad (when)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.Trans (MonadTrans(lift))
import qualified Data.Foldable as Foldable
import Data.Functor ((<$>))
import Data.Hashable (Hashable)
import Data.Maybe (listToMaybe)
import qualified Data.Sequence as Seq
import Data.Traversable (Traversable, traverse)

import Control.Monad.Ask.Class (MonadAsk, ask)
import Control.Monad.Memo.Class
import Control.Monad.Quantum.Base.Class
import Control.Monad.Quantum.Toffoli.Class
import Control.Monad.Quantum.Class
import Control.Monad.Quantum.CircuitBuilder.Class
import Control.Monad.Quantum.Counter.Class
import Data.Quantum.Circuit (IsCircuit)
import Data.Quantum.Wire (Wire)

-- |Add support for `MonadQuantum` on top of a `MonadToffoli`.

newtype QuantumControlT w m a = QuantumControlT {
  unwrapQuantumControlT :: ReaderT [w] m a
} deriving (Functor, Applicative, Monad)

runQuantumControlT :: QuantumControlT w m a -> [w] -> m a
runQuantumControlT = runReaderT . unwrapQuantumControlT

instance MonadTrans (QuantumControlT w) where
  lift = QuantumControlT . lift

liftHandleMaybeCtrl :: MonadToffoli w m => (Maybe w -> m a) -> QuantumControlT w m a
liftHandleMaybeCtrl body =
    compressCtrls 1 $ QuantumControlT $ ReaderT (body . listToMaybe)

instance MonadToffoli w m => MonadQuantumBase w (QuantumControlT w m) where

  newWire =
      lift newWire
  {-# INLINABLE newWire #-}

  ancilla =
      lift ancilla
  {-# INLINABLE ancilla #-}

  applyX w =
      compressCtrls 2 $ QuantumControlT $ ReaderT $ \ctrls ->
        case ctrls of
          [] ->
            applyX w
          [w1] ->
            rawCnotWire w w1 False
          [w1, w2] ->
            with_ (applyH w) $ rawApplyZCC w False w1 False w2 False
          _ ->
            error "applyX: compressCtrls does not compress the control context"
  {-# INLINABLE applyX #-}

  applyY w = do
      ctrls <- askCtrls
      if null ctrls then
        lift $ applyY w
      else
        with_ (lift $ rawApplyS w False True) (applyX w)
  {-# INLINABLE applyY #-}

  rawApplyZ w neg =
      compressCtrls 2 $ QuantumControlT $ ReaderT $ \ctrls ->
        case ctrls of
          [] ->
            rawApplyZ w neg
          [w1] ->
            with_ (do
                when neg $ applyX w
                applyH w) $
              rawCnotWire w w1 False
          [w1, w2] ->
            rawApplyZCC w neg w1 False w2 False
          _ ->
            error "rawApplyZ: compressCtrls does not compress the control context"
  {-# INLINABLE rawApplyZ #-}

  applyH w =
      liftHandleMaybeCtrl $ \ctrl ->
        case ctrl of
          Nothing ->
            applyH w
          Just w1 -> do
            -- The circuit in Figure 7 of Amy, Maslov, Mosca, and Roetteler (arXiv:1206.0758v1 [quant-ph])
            parallel_ (applyH w1) (rawApplyS w False True)
            cnotWire w1 (bit w)
            cnotWire w (bit w1)
            parallel_ (applyH w1) (applyH w)
            parallel_ (rawApplyT w1 False True) (rawApplyT w False False)
            parallel_ (applyH w1) (applyH w)
            cnotWire w (bit w1)
            parallel_ (applyH w1) (rawApplyS w False False)
  {-# INLINABLE applyH #-}

  rawApplyS w neg inv =
      rawControl w neg $ rotateGlobalPhase90 inv
  {-# INLINABLE rawApplyS #-}

  rawApplyT w neg inv =
      rawControl w neg $ rotateGlobalPhase45 inv
  {-# INLINABLE rawApplyT #-}

  negateGlobalPhase =
      liftHandleMaybeCtrl $ \ctrl ->
        case ctrl of
          Nothing ->
            return ()
          Just ctrlwire ->
            rawApplyZ ctrlwire False
  {-# INLINABLE negateGlobalPhase #-}

  rotateGlobalPhase90 inv =
      liftHandleMaybeCtrl $ \ctrl ->
        case ctrl of
          Nothing ->
            return ()
          Just ctrlwire ->
            rawApplyS ctrlwire False inv
  {-# INLINABLE rotateGlobalPhase90 #-}

  rotateGlobalPhase45 inv =
      liftHandleMaybeCtrl $ \ctrl ->
        case ctrl of
          Nothing ->
            return ()
          Just ctrlwire ->
            rawApplyT ctrlwire False inv
  {-# INLINABLE rotateGlobalPhase45 #-}

  rawApplyZC a nega b negb =
      rawControl a nega $ rawApplyZ b negb
  {-# INLINABLE rawApplyZC #-}

  rawCnotWire targ ctrl neg =
      rawControl ctrl neg $ applyX targ
  {-# INLINABLE rawCnotWire #-}

  rawCnotWires [] =
      return ()
  rawCnotWires [(targ, ctrl, neg)] =
      rawCnotWire targ ctrl neg
  rawCnotWires ws =
      liftHandleMaybeCtrl $ \ctrl' ->
        case ctrl' of
          Nothing ->
            mapParM_ (\(targ, ctrl, neg) -> rawCnotWire targ ctrl neg) ws
          Just c ->
            with (traverse (\(a, b, negb) -> rawPrepareForXCC a b negb c) ws) $ \tmps ->
              parallels_ (rawApplyTs c tmps)
    where
      rawPrepareForXCC a b negb c = do
          when negb $ applyX b
          ab <- ancilla
          ac <- ancilla
          bc <- ancilla
          abc <- ancilla
          applyH a
          cnotWire ab (bit a)
          cnotWire ac (bit a)
          cnotWire abc (bit a)
          cnotWire ab (bit b)
          cnotWire bc (bit b)
          cnotWire abc (bit b)
          cnotWire ac (bit c)
          cnotWire bc (bit c)
          cnotWire abc (bit c)
          return [(a, False), (b, False), (ab, True), (ac, True), (bc, True), (abc, False)]
      rawApplyTs c tmps =
          if even (length ws) then
            rest
          else
            rawApplyT c False False : rest
        where
          rest =
              concat $ zipWith f [0 :: Int ..] tmps
            where
              f i pairs =
                  if even i then
                    (\(w, inv) -> rawApplyT w False inv) <$> pairs
                  else
                    (\(w, inv) -> rawApplyT w False (not inv)) <$> pairs
  {-# INLINABLE rawCnotWires #-}

  with prepare body = QuantumControlT $ ReaderT $ \ctrls ->
      with (runQuantumControlT prepare []) (\a -> runQuantumControlT (body a) ctrls)
  {-# INLINABLE with #-}

  -- Apply given two circuits in parallel if the current control context is empty.
  -- Otherwise, apply the circuits sequentially.
  bindParallel ma mb =
      compressCtrls 1 $ do
        ctrls <- askCtrls
        if null ctrls then
          lift $ bindParallel (runQuantumControlT ma []) (\a -> runQuantumControlT (mb a) [])
        else
          ma >>= mb
  {-# INLINABLE bindParallel #-}

  invert body = QuantumControlT $ ReaderT $ \ctrls ->
      invert (runQuantumControlT body ctrls)
  {-# INLINABLE invert #-}

  withoutCtrls body = QuantumControlT $ ReaderT $ \_ ->
      runQuantumControlT body []
  {-# INLINABLE withoutCtrls #-}

  replicateQ 0 _ = return Seq.empty
  replicateQ 1 m = Seq.singleton <$> m
  replicateQ n m = compressCtrls 1 $
      QuantumControlT $ ReaderT $ \ctrls ->
        replicateQ n (runQuantumControlT m ctrls)
  {-# INLINABLE replicateQ #-}

  -- The composition is in parallel if the current control context is empty, and is sequential otherwise.
  replicateParallelQ n m = do
      ctrls <- askCtrls
      if null ctrls then
        lift $ replicateParallelQ n (runQuantumControlT m [])
      else
        replicateQ n m
  {-# INLINABLE replicateParallelQ #-}

  replicateQ_ 0 _ = return ()
  replicateQ_ 1 m = m
  replicateQ_ n m = compressCtrls 1 $
      QuantumControlT $ ReaderT $ \ctrls ->
        replicateQ_ n (runQuantumControlT m ctrls)
  {-# INLINABLE replicateQ_ #-}

  -- The composition is in parallel if the current control context is empty, and is sequential otherwise.
  replicateParallelQ_ n m = do
      ctrls <- askCtrls
      if null ctrls then
        lift $ replicateParallelQ_ n (runQuantumControlT m [])
      else
        replicateQ_ n m
  {-# INLINABLE replicateParallelQ_ #-}

  genericReplicateQ_ 0 _ = return ()
  genericReplicateQ_ 1 m = m
  genericReplicateQ_ n m = compressCtrls 1 $
      QuantumControlT $ ReaderT $ \ctrls ->
        genericReplicateQ_ n (runQuantumControlT m ctrls)
  {-# INLINABLE genericReplicateQ_ #-}

  genericReplicateParallelQ_ n m = do
      ctrls <- askCtrls
      if null ctrls then
        lift $ genericReplicateParallelQ_ n (runQuantumControlT m [])
      else
        genericReplicateQ_ n m
  {-# INLINABLE genericReplicateParallelQ_ #-}

instance MonadToffoli w m => MonadToffoli w (QuantumControlT w m) where

  rawApplyXCC a b negb c negc =
      rawControl b negb $ rawControl c negc $ applyX a
  {-# INLINABLE rawApplyXCC #-}

  rawApplyZCC a nega b negb c negc =
      rawControl b negb $ rawControl c negc $ rawApplyZ a nega
  {-# INLINABLE rawApplyZCC #-}

  rawDestructiveToffoli a b negb c negc = QuantumControlT $ ReaderT $ \ctrls ->
      if null ctrls then
        rawDestructiveToffoli a b negb c negc
      else
        runQuantumControlT (rawApplyXCC a b negb c negc) ctrls
  {-# INLINABLE rawDestructiveToffoli #-}

instance MonadToffoli w m => MonadQuantum w (QuantumControlT w m) where

  handleCtrls handler = QuantumControlT $ ReaderT $ \ctrls ->
      runQuantumControlT (handler ctrls) []
  {-# INLINABLE handleCtrls #-}

  rawControl ctrlwire neg (QuantumControlT m) =
      with_ (when neg (applyX ctrlwire)) $
        QuantumControlT $ Reader.local (ctrlwire :) m
  {-# INLINABLE rawControl #-}

  compressCtrls n (QuantumControlT body)
    | n > 0 =
        handleCtrls $ \ctrls ->
          with (reduceCtrls n $ Seq.fromList ctrls) $ \ctrls' ->
            QuantumControlT $ Reader.local (const (Foldable.toList ctrls')) body
    | otherwise =
        error "compressCtrls: n must be positive"
  {-# INLINABLE compressCtrls #-}

instance MonadAsk r m => MonadAsk r (QuantumControlT w m) where
  ask = lift ask
  {-# INLINABLE ask #-}

instance MonadMemo (k, [w]) m => MonadMemo k (QuantumControlT w m) where
  memoize k m = QuantumControlT $ ReaderT $ \ctrls ->
      memoize (k, ctrls) (runQuantumControlT m ctrls)
  {-# INLINABLE memoize #-}

instance (IsCircuit g Wire c, MonadToffoli Wire m, MonadCircuitBuilder c m) => MonadCircuitBuilder c (QuantumControlT Wire m) where
  recordGate g touched = QuantumControlT $ ReaderT $ \ctrls ->
      if null ctrls then
        recordGate g touched
      else
        error "QuantumControlT.recordGate: Control context must be empty"

deriving instance (Eq w, Hashable w, MonadToffoli w m, MonadQuantumCounter w c m) => MonadQuantumCounter w c (QuantumControlT w m)
