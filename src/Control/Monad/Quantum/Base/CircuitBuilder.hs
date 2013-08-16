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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Quantum.Base.CircuitBuilder (
    CircuitBuilderBase, runCircuitBuilderBase, evalCircuitBuilderBase, evalCircuitBuilderBase_, execCircuitBuilderBase,
    buildCircuitBase) where

import Control.Applicative (Applicative(..))
import Control.Monad (when)
import Control.Monad.State.Strict (MonadState, StateT(StateT), runStateT, evalStateT, execStateT)
import qualified Control.Monad.State as State
import Control.Monad.Writer.Strict (Writer, writer, runWriter)
import qualified Control.Monad.Writer as Writer
import qualified Data.Foldable as Foldable
import Data.Monoid (Monoid, mempty, (<>))
import qualified Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Quantum.Circuit.Class (Wire(..), IsGate(..), IsCircuit)
import qualified Data.Quantum.Circuit.Class as Circuit
import Data.Quantum.Circuit.Invert (IsCircuitInvert, CircuitInvert, runCircuitInvert)
import qualified Data.Quantum.Circuit.Invert as Circuit
import Data.Quantum.Circuit.MinimizeWireIndices
import Control.Monad.Quantum.Base.Class
import Control.Monad.Quantum.CircuitBuilder.Class
import Control.Monad.Quantum.Toffoli.Class
import Util (disjointAscLists)

data TouchedCircuit c = TouchedCircuit c (Set Integer)

instance IsCircuit g Wire c => IsCircuit g Wire (TouchedCircuit c) where

  empty = TouchedCircuit Circuit.empty Set.empty
  {-# INLINABLE empty #-}

  singleton g = TouchedCircuit (Circuit.singleton g) (Set.fromList $ fmap getWire $ Foldable.toList g)
  {-# INLINABLE singleton #-}
  newWire w inv = TouchedCircuit (Circuit.newWire w inv) (Set.singleton $ getWire w)
  {-# INLINABLE newWire #-}
  ancilla w inv = TouchedCircuit (Circuit.ancilla w inv) (Set.singleton $ getWire w)
  {-# INLINABLE ancilla #-}

  sequential (TouchedCircuit c1 touched1) (TouchedCircuit c2 touched2) =
      TouchedCircuit (Circuit.sequential c1 c2) (Set.union touched1 touched2)
  {-# INLINABLE sequential #-}

  parallel (TouchedCircuit c1 touched1) (TouchedCircuit c2 touched2) =
      TouchedCircuit (Circuit.parallel c1 c2) (Set.union touched1 touched2)
  {-# INLINABLE parallel #-}

instance IsCircuitInvert g Wire c => IsCircuitInvert g Wire (TouchedCircuit c) where

  invert (TouchedCircuit c touched) =
      TouchedCircuit (Circuit.invert c) touched
  {-# INLINABLE invert #-}

instance IsCircuit g Wire c => Monoid (TouchedCircuit c) where
  mempty = Circuit.empty
  {-# INLINABLE mempty #-}
  mappend = Circuit.sequential
  {-# INLINABLE mappend #-}

-- | @CircuitBuilderBase@ is a monad which maintains the current control context, the next wire index,
-- and the sequence of elementary gates.
newtype CircuitBuilderBase c a = CircuitBuilderBase {
  unwrapCircuitBuilderBase :: StateT Integer (Writer (TouchedCircuit c)) a
} deriving Functor

-- The following standalone newtype derivings cause panic in GHC 7.4.1:
-- deriving instance IsCircuit g Wire c => Applicative (CircuitBuilderBase c)
-- deriving instance IsCircuit g Wire c => Monad (CircuitBuilderBase c)
-- The reduced testcase is in https://gist.github.com/3626747.

instance IsCircuit g Wire c => Applicative (CircuitBuilderBase c) where
  pure = CircuitBuilderBase . pure
  {-# INLINABLE pure #-}
  CircuitBuilderBase f <*> CircuitBuilderBase a =
      CircuitBuilderBase (f <*> a)
  {-# INLINABLE (<*>) #-}

instance IsCircuit g Wire c => Monad (CircuitBuilderBase c) where
  return = CircuitBuilderBase . return
  {-# INLINABLE return #-}
  CircuitBuilderBase m >>= f =
      CircuitBuilderBase (m >>= unwrapCircuitBuilderBase . f)
  {-# INLINABLE (>>=) #-}

nextWire :: MonadState Integer m => m Wire
nextWire = do
    wi <- State.get
    State.put $! wi + 1
    return $ Wire wi
{-# INLINABLE nextWire #-}

instance IsCircuitInvert g Wire c => MonadQuantumBase Wire (CircuitBuilderBase c) where

  newWire = CircuitBuilderBase $ do
      w <- nextWire
      Writer.tell $ Circuit.newWire w False
      return w
  {-# INLINABLE newWire #-}

  ancilla = CircuitBuilderBase $ do
      w <- nextWire
      Writer.tell $ Circuit.ancilla w False
      return w
  {-# INLINABLE ancilla #-}

  applyX w = CircuitBuilderBase $
      Writer.tell $ Circuit.singleton $ gateX w
  {-# INLINABLE applyX #-}

  applyY w = CircuitBuilderBase $
      Writer.tell $ Circuit.singleton $ gateY w
  {-# INLINABLE applyY #-}

  rawApplyZ w _neg = CircuitBuilderBase $
      Writer.tell $ Circuit.singleton $ gateZ w
  {-# INLINABLE rawApplyZ #-}

  applyH w = CircuitBuilderBase $
      Writer.tell $ Circuit.singleton $ gateH w
  {-# INLINABLE applyH #-}

  rawApplyS w neg inv = CircuitBuilderBase $
      Writer.tell $ Circuit.singleton $ gateS w (neg /= inv)
  {-# INLINABLE rawApplyS #-}

  rawApplyT w neg inv = CircuitBuilderBase $
      Writer.tell $ Circuit.singleton $ gateT w (neg /= inv)
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

  rawCnotWire w w1 neg = CircuitBuilderBase $
      if w == w1 then
        error "rawCnotWire: wires must be distinct"
      else
        Writer.tell $ Circuit.singleton $ gateXC w w1 neg
  {-# INLINABLE rawCnotWire #-}

  rawCnotWires ws =
      mapParM_ (\(w, w1, neg) -> rawCnotWire w w1 neg) ws
  {-# INLINABLE rawCnotWires #-}

  with prepare body = CircuitBuilderBase $ StateT $ \nextWireIndex ->
      let ((a, nextWireIndex'), prepareCircuit) = runWriter $ runStateT (unwrapCircuitBuilderBase prepare) nextWireIndex
          ((b, nextWireIndex''), bodyCircuit) = runWriter $ runStateT (unwrapCircuitBuilderBase $ body a) nextWireIndex'
      in
      writer ((b, nextWireIndex''), prepareCircuit <> bodyCircuit <> Circuit.invert prepareCircuit)
  {-# INLINABLE with #-}

  bindParallel m1 m2 = CircuitBuilderBase $ StateT $ \nextWireIndex ->
      let ((a1, nextWireIndex'), circuit1@(TouchedCircuit _ touched1)) = runWriter $ runStateT (unwrapCircuitBuilderBase m1) nextWireIndex
          ((a2, nextWireIndex''), circuit2@(TouchedCircuit _ touched2)) = runWriter $ runStateT (unwrapCircuitBuilderBase $ m2 a1) nextWireIndex'
      in
      if disjointAscLists (Set.toAscList touched1) (Set.toAscList touched2) then
        writer ((a2, nextWireIndex''), Circuit.parallel circuit1 circuit2)
      else
        error "bindParallel: subcircuits touch the same wire"
  {-# INLINABLE bindParallel #-}

  invert body = CircuitBuilderBase $ StateT $ \nextWireIndex ->
      let ((a, nextWireIndex'), circuit) = runWriter $ runStateT (unwrapCircuitBuilderBase body) nextWireIndex in
      writer ((a, nextWireIndex'), Circuit.invert circuit)
  {-# INLINABLE invert #-}

  withoutCtrls m = m
  {-# INLINABLE withoutCtrls #-}

instance IsCircuitInvert g Wire c => MonadToffoli Wire (CircuitBuilderBase c) where

  rawApplyXCC w w1 neg1 w2 neg2 =
      if w == w1 || w == w2 || w1 == w2 then
        error "rawApplyXCC: wires must be distinct"
      else
        CircuitBuilderBase $ Writer.tell $ Circuit.singleton $ gateXCC w w1 neg1 w2 neg2
  {-# INLINABLE rawApplyXCC #-}

  rawApplyZCC a nega b negb c negc =
      with_ (do
          when nega $ applyX a
          applyH a) $
        rawApplyXCC a b negb c negc
  {-# INLINABLE rawApplyZCC #-}

  rawDestructiveToffoli = rawApplyXCC
  {-# INLINABLE rawDestructiveToffoli #-}

instance IsCircuitInvert g Wire c => MonadCircuitBuilder c (CircuitBuilderBase c) where
  recordGate c touched = CircuitBuilderBase $ Writer.tell (TouchedCircuit c touched)
  {-# INLINABLE recordGate #-}

runCircuitBuilderBase :: CircuitBuilderBase c a -> Integer -> (a, Integer, c)
runCircuitBuilderBase (CircuitBuilderBase m) nextWireIndex =
    (a, nextWireIndex', c)
  where
    ((a, nextWireIndex'), TouchedCircuit c _) = runWriter $ runStateT m nextWireIndex

evalCircuitBuilderBase :: IsCircuit g Wire c => CircuitBuilderBase c a -> Integer -> (a, c)
evalCircuitBuilderBase (CircuitBuilderBase m) nextWireIndex =
    (a, c)
  where
    (a, TouchedCircuit c _) = runWriter $ evalStateT m nextWireIndex

evalCircuitBuilderBase_ :: IsCircuit g Wire c => CircuitBuilderBase c a -> Integer -> c
evalCircuitBuilderBase_ m nextWireIndex =
    snd $ evalCircuitBuilderBase m nextWireIndex

execCircuitBuilderBase :: IsCircuit g Wire c => CircuitBuilderBase c a -> Integer -> (Integer, c)
execCircuitBuilderBase (CircuitBuilderBase m) nextWireIndex =
    (nextWireIndex', c)
  where
    (nextWireIndex', TouchedCircuit c _) = runWriter $ execStateT m nextWireIndex

buildCircuitBase :: IsCircuit g Wire c => CircuitBuilderBase (CircuitInvert (MinimizeWireIndices c)) a -> c
buildCircuitBase m =
    minimizeWireIndices $ runCircuitInvert $ evalCircuitBuilderBase_ m 0
