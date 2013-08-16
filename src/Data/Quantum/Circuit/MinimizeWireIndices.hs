{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Quantum.Circuit.MinimizeWireIndices (
    MinimizeWireIndices, minimizeWireIndices) where

import Control.Applicative (Applicative)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)
import Control.Monad.Writer.Strict (Writer, runWriter)
import qualified Control.Monad.Writer.Strict as Writer
import qualified Data.Foldable as Foldable
import Data.Functor ((<$>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Monoid, mempty, (<>))
import qualified Data.Monoid as Monoid
import Data.Set (Set)
import qualified Data.Set as Set

import Data.MexSet (MexSet)
import qualified Data.MexSet as MexSet
import Data.Quantum.Circuit.Class
import Util (writer')

data MinimizationState = MinimizationState {
  _msTouched :: !(Set Integer),
  _msDeleted :: !(Map Integer Integer),
  _msInserted :: !(Map Integer Integer)
}

instance Monoid MinimizationState where
  mempty = MinimizationState Set.empty Map.empty Map.empty
  MinimizationState touched1 del1 ins1 `mappend` MinimizationState touched2 del2 ins2 =
      MinimizationState touched del ins
    where
      touched = Set.union touched1 touched2
      del = Map.union del1 (Map.difference del2 ins1)
      ins = Map.union (Map.difference ins1 del2) ins2

newtype MinimizeWireIndices a = MinimizeWireIndices {
  unwrapMinimizeWireIndices :: ReaderT (MexSet Integer, HashMap Integer Integer) (Writer MinimizationState) a
} deriving (Functor, Applicative, Monad)

minimizeWireIndices :: IsCircuit g Wire c => MinimizeWireIndices c -> c
minimizeWireIndices = fst . runWriter . flip runReaderT (MexSet.empty, HashMap.empty) . unwrapMinimizeWireIndices
{-# INLINABLE minimizeWireIndices #-}

instance IsCircuit g Wire c => IsCircuit g Wire (MinimizeWireIndices c) where

  empty = return empty
  {-# INLINABLE empty #-}
  singleton g = MinimizeWireIndices $ ReaderT $ \(!_, !dict) ->
      let g' = lookupWire dict <$> g
          c' = singleton g' in
      writer' (c', MinimizationState (Set.fromList $ fmap getWire $ Foldable.toList g') Map.empty Map.empty)
  {-# INLINABLE singleton #-}
  newWire w False = MinimizeWireIndices $ ReaderT $ \(!inUse, !_) -> do
      w' <- insertWireIndex inUse w
      let c' = newWire w' False
      writer' (c', MinimizationState (Set.singleton $ getWire w') Map.empty Map.empty)
  newWire w True = MinimizeWireIndices $ ReaderT $ \(!_, !dict) -> do
      let w' = lookupWire dict w
          c' = newWire w' True
      deleteWireIndex dict w
      writer' (c', MinimizationState (Set.singleton $ getWire w') Map.empty Map.empty)
  {-# INLINABLE newWire #-}
  ancilla w False = MinimizeWireIndices $ ReaderT $ \(!inUse, !_) -> do
      w' <- insertWireIndex inUse w
      let c' = ancilla w' False
      writer' (c', MinimizationState (Set.singleton $ getWire w') Map.empty Map.empty)
  ancilla w True = MinimizeWireIndices $ ReaderT $ \(!_, !dict) -> do
      let w' = lookupWire dict w
          c' = ancilla w' True
      deleteWireIndex dict w
      writer' (c', MinimizationState (Set.singleton $ getWire w') Map.empty Map.empty)
  {-# INLINABLE ancilla #-}

  sequential (MinimizeWireIndices m1) (MinimizeWireIndices m2) = MinimizeWireIndices $ ReaderT $ \(!inUse, !dict) -> writer' $
      let (c1', ms1@(MinimizationState _ del1 ins1)) = runWriter $ runReaderT m1 (inUse, dict)
          (c2', ms2) = runWriter $ runReaderT m2 (updateInUse del1 ins1 inUse, updateDict del1 ins1 dict) in
      (sequential c1' c2', ms1 <> ms2)
  {-# INLINABLE sequential #-}

  parallel (MinimizeWireIndices m1) (MinimizeWireIndices m2) = MinimizeWireIndices $ ReaderT $ \(!inUse, !dict) -> writer' $
      let (c1', ms1@(MinimizationState touched1 del1 ins1)) = runWriter $ runReaderT m1 (inUse, dict)
          (c2', ms2) = runWriter $ runReaderT m2 (foldr MexSet.insert inUse (Set.elems touched1), updateDict del1 ins1 dict) in
      (parallel c1' c2', ms1 <> ms2)
  {-# INLINABLE parallel #-}

lookupWire :: HashMap Integer Integer -> Wire -> Wire
lookupWire dict (Wire wi) =
    Wire $ HashMap.lookupDefault (error "minimizeWireIndicesGateList: Wire index used without allocation") wi dict
insertWireIndex :: MexSet Integer -> Wire -> Writer MinimizationState Wire
insertWireIndex inUse (Wire wi) = writer' $
    let wi' = MexSet.mex inUse in
    (Wire wi', MinimizationState Set.empty Map.empty (Map.singleton wi' wi))
    -- (MexSet.insert wi' inUse, HashMap.insert wi wi' dict)
deleteWireIndex :: HashMap Integer Integer -> Wire -> Writer MinimizationState ()
deleteWireIndex dict (Wire wi) =
    let wi' = HashMap.lookupDefault (error "minimizeWireIndicesGateList: Wire index used without allocation") wi dict in
    Writer.tell $! MinimizationState Set.empty (Map.singleton wi' wi) Map.empty
    -- State.put (MexSet.delete wi' inUse, HashMap.delete wi dict)
updateInUse :: Map Integer Integer -> Map Integer Integer -> MexSet Integer -> MexSet Integer
updateInUse del ins inUse =
    foldr MexSet.insert (foldr MexSet.delete inUse (Map.keys del)) (Map.keys ins)
updateDict :: Map Integer Integer -> Map Integer Integer -> HashMap Integer Integer -> HashMap Integer Integer
updateDict del ins dict =
    foldr (uncurry $ flip HashMap.insert) (foldr HashMap.delete dict (Map.elems del)) (Map.assocs ins)
