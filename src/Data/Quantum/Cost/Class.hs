{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE DeriveDataTypeable #-}

module Data.Quantum.Cost.Class (Cost(..), WrappedCost(..)) where

import Data.Monoid (Monoid)
import qualified Data.Monoid
import Data.Typeable (Typeable)
import Data.Quantum.CountKey (GateKey)

class Cost c where
  empty :: c
  newWire :: c
  gate :: GateKey -> c
  -- |Combine the costs of two circuits applied sequentially.
  sequential :: c -> c -> c
  -- |Combine the costs of two circuits applied in parallel.
  parallel :: c -> c -> c
  -- |Combine the costs of two circuits in the "with" construct.
  with :: c -> c -> c
  -- |The cost of the inverse of a circuit.
  adjoint :: c -> c
  -- |The cost of the sequential repetition of a circuit.
  replicateSequential :: Int -> c -> c
  replicateSequential = genericReplicateSequential
  {-# INLINABLE replicateSequential #-}
  -- |The generic version of `replicateSequential`.
  genericReplicateSequential :: Integral i => i -> c -> c
  -- |The cost of the parallel repetition of a circuit.
  replicateParallel :: Int -> c -> c
  replicateParallel = genericReplicateParallel
  {-# INLINABLE replicateParallel #-}
  -- |The generic version of `replicateParallel`.
  genericReplicateParallel :: Integral i => i -> c -> c

newtype WrappedCost c = WrappedCost {
  unwrapCost :: c
} deriving (Eq, Typeable)

-- Monoid Wrapper for Cost
--
-- We do not make Cost a subclass of Monoid because the Monoid instances for tuples
-- in Data.Monoid are lazy whereas we want strict implementations.

instance Cost c => Monoid (WrappedCost c) where
  mempty = WrappedCost empty
  {-# INLINE mempty #-}
  WrappedCost a `mappend` WrappedCost b = WrappedCost $ sequential a b
  {-# INLINE mappend #-}

-- Instances for tuples
--
-- When a tuple is used as the log type of a Writer monad,
-- we usually want to synchronize the computation of all components of a tuple.
-- Therefore, we define these instances with seq.
-- I decided not to introduce types for strict tuples after reading a discussion
-- about strict tuple types on haskell-prime mailing list in March 2006,
-- in particular the following message by John Meacham:
-- <http://www.haskell.org/pipermail/haskell-prime/2006-March/001006.html>.

strictPair :: a1 -> a2 -> (a1, a2)
strictPair a1 a2 =
    a1 `seq` a2 `seq` (a1, a2)

instance (Cost c1, Cost c2) => Cost (c1, c2) where
  empty =
      strictPair empty empty
  {-# INLINABLE empty #-}
  newWire =
      strictPair newWire newWire
  {-# INLINABLE newWire #-}
  gate key =
      strictPair (gate key) (gate key)
  {-# INLINABLE gate #-}
  sequential (c1, c2) (c1', c2') =
      strictPair (sequential c1 c1') (sequential c2 c2')
  {-# INLINABLE sequential #-}
  parallel (c1, c2) (c1', c2') =
      strictPair (parallel c1 c1') (parallel c2 c2')
  {-# INLINABLE parallel #-}
  with (c1, c2) (c1', c2') =
      strictPair (with c1 c1') (with c2 c2')
  {-# INLINABLE with #-}
  adjoint (c1, c2) =
      strictPair (adjoint c1) (adjoint c2)
  {-# INLINABLE adjoint #-}
  replicateSequential n (c1, c2) =
      strictPair (replicateSequential n c1) (replicateSequential n c2)
  {-# INLINABLE replicateSequential #-}
  genericReplicateSequential n (c1, c2) =
      strictPair (genericReplicateSequential n c1) (genericReplicateSequential n c2)
  {-# INLINABLE genericReplicateSequential #-}
  replicateParallel n (c1, c2) =
      strictPair (replicateParallel n c1) (replicateParallel n c2)
  {-# INLINABLE replicateParallel #-}
  genericReplicateParallel n (c1, c2) =
      strictPair (genericReplicateParallel n c1) (genericReplicateParallel n c2)
  {-# INLINABLE genericReplicateParallel #-}

strictTuple3 :: a1 -> a2 -> a3 -> (a1, a2, a3)
strictTuple3 a1 a2 a3 =
    a1 `seq` a2 `seq` a3 `seq` (a1, a2, a3)

instance (Cost c1, Cost c2, Cost c3) => Cost (c1, c2, c3) where
  empty =
      strictTuple3 empty empty empty
  {-# INLINABLE empty #-}
  newWire =
      strictTuple3 newWire newWire newWire
  {-# INLINABLE newWire #-}
  gate key =
      strictTuple3 (gate key) (gate key) (gate key)
  {-# INLINABLE gate #-}
  sequential (c1, c2, c3) (c1', c2', c3') =
      strictTuple3 (sequential c1 c1') (sequential c2 c2') (sequential c3 c3')
  {-# INLINABLE sequential #-}
  parallel (c1, c2, c3) (c1', c2', c3') =
      strictTuple3 (parallel c1 c1') (parallel c2 c2') (parallel c3 c3')
  {-# INLINABLE parallel #-}
  with (c1, c2, c3) (c1', c2', c3') =
      strictTuple3 (with c1 c1') (with c2 c2') (with c3 c3')
  {-# INLINABLE with #-}
  adjoint (c1, c2, c3) =
      strictTuple3 (adjoint c1) (adjoint c2) (adjoint c3)
  {-# INLINABLE adjoint #-}
  replicateSequential n (c1, c2, c3) =
      strictTuple3 (replicateSequential n c1) (replicateSequential n c2) (replicateSequential n c3)
  {-# INLINABLE replicateSequential #-}
  genericReplicateSequential n (c1, c2, c3) =
      strictTuple3 (genericReplicateSequential n c1) (genericReplicateSequential n c2) (genericReplicateSequential n c3)
  {-# INLINABLE genericReplicateSequential #-}
  replicateParallel n (c1, c2, c3) =
      strictTuple3 (replicateParallel n c1) (replicateParallel n c2) (replicateParallel n c3)
  {-# INLINABLE replicateParallel #-}
  genericReplicateParallel n (c1, c2, c3) =
      strictTuple3 (genericReplicateParallel n c1) (genericReplicateParallel n c2) (genericReplicateParallel n c3)
  {-# INLINABLE genericReplicateParallel #-}

strictTuple4 :: a1 -> a2 -> a3 -> a4 -> (a1, a2, a3, a4)
strictTuple4 a1 a2 a3 a4 =
    a1 `seq` a2 `seq` a3 `seq` a4 `seq` (a1, a2, a3, a4)

instance (Cost c1, Cost c2, Cost c3, Cost c4) => Cost (c1, c2, c3, c4) where
  empty =
      strictTuple4 empty empty empty empty
  {-# INLINABLE empty #-}
  newWire =
      strictTuple4 newWire newWire newWire newWire
  {-# INLINABLE newWire #-}
  gate key =
      strictTuple4 (gate key) (gate key) (gate key) (gate key)
  {-# INLINABLE gate #-}
  sequential (c1, c2, c3, c4) (c1', c2', c3', c4') =
      strictTuple4 (sequential c1 c1') (sequential c2 c2') (sequential c3 c3') (sequential c4 c4')
  {-# INLINABLE sequential #-}
  parallel (c1, c2, c3, c4) (c1', c2', c3', c4') =
      strictTuple4 (parallel c1 c1') (parallel c2 c2') (parallel c3 c3') (parallel c4 c4')
  {-# INLINABLE parallel #-}
  with (c1, c2, c3, c4) (c1', c2', c3', c4') =
      strictTuple4 (with c1 c1') (with c2 c2') (with c3 c3') (with c4 c4')
  {-# INLINABLE with #-}
  adjoint (c1, c2, c3, c4) =
      strictTuple4 (adjoint c1) (adjoint c2) (adjoint c3) (adjoint c4)
  {-# INLINABLE adjoint #-}
  replicateSequential n (c1, c2, c3, c4) =
      strictTuple4 (replicateSequential n c1) (replicateSequential n c2) (replicateSequential n c3) (replicateSequential n c4)
  {-# INLINABLE replicateSequential #-}
  genericReplicateSequential n (c1, c2, c3, c4) =
      strictTuple4 (genericReplicateSequential n c1) (genericReplicateSequential n c2) (genericReplicateSequential n c3) (genericReplicateSequential n c4)
  {-# INLINABLE genericReplicateSequential #-}
  replicateParallel n (c1, c2, c3, c4) =
      strictTuple4 (replicateParallel n c1) (replicateParallel n c2) (replicateParallel n c3) (replicateParallel n c4)
  {-# INLINABLE replicateParallel #-}
  genericReplicateParallel n (c1, c2, c3, c4) =
      strictTuple4 (genericReplicateParallel n c1) (genericReplicateParallel n c2) (genericReplicateParallel n c3) (genericReplicateParallel n c4)
  {-# INLINABLE genericReplicateParallel #-}

strictTuple5 :: a1 -> a2 -> a3 -> a4 -> a5 -> (a1, a2, a3, a4, a5)
strictTuple5 a1 a2 a3 a4 a5 =
    a1 `seq` a2 `seq` a3 `seq` a4 `seq` a5 `seq` (a1, a2, a3, a4, a5)

instance (Cost c1, Cost c2, Cost c3, Cost c4, Cost c5) => Cost (c1, c2, c3, c4, c5) where
  empty =
      strictTuple5 empty empty empty empty empty
  {-# INLINABLE empty #-}
  newWire =
      strictTuple5 newWire newWire newWire newWire newWire
  {-# INLINABLE newWire #-}
  gate key =
      strictTuple5 (gate key) (gate key) (gate key) (gate key) (gate key)
  {-# INLINABLE gate #-}
  sequential (c1, c2, c3, c4, c5) (c1', c2', c3', c4', c5') =
      strictTuple5 (sequential c1 c1') (sequential c2 c2') (sequential c3 c3') (sequential c4 c4') (sequential c5 c5')
  {-# INLINABLE sequential #-}
  parallel (c1, c2, c3, c4, c5) (c1', c2', c3', c4', c5') =
      strictTuple5 (parallel c1 c1') (parallel c2 c2') (parallel c3 c3') (parallel c4 c4') (parallel c5 c5')
  {-# INLINABLE parallel #-}
  with (c1, c2, c3, c4, c5) (c1', c2', c3', c4', c5') =
      strictTuple5 (with c1 c1') (with c2 c2') (with c3 c3') (with c4 c4') (with c5 c5')
  {-# INLINABLE with #-}
  adjoint (c1, c2, c3, c4, c5) =
      strictTuple5 (adjoint c1) (adjoint c2) (adjoint c3) (adjoint c4) (adjoint c5)
  {-# INLINABLE adjoint #-}
  replicateSequential n (c1, c2, c3, c4, c5) =
      strictTuple5 (replicateSequential n c1) (replicateSequential n c2) (replicateSequential n c3) (replicateSequential n c4) (replicateSequential n c5)
  {-# INLINABLE replicateSequential #-}
  genericReplicateSequential n (c1, c2, c3, c4, c5) =
      strictTuple5 (genericReplicateSequential n c1) (genericReplicateSequential n c2) (genericReplicateSequential n c3) (genericReplicateSequential n c4) (genericReplicateSequential n c5)
  {-# INLINABLE genericReplicateSequential #-}
  replicateParallel n (c1, c2, c3, c4, c5) =
      strictTuple5 (replicateParallel n c1) (replicateParallel n c2) (replicateParallel n c3) (replicateParallel n c4) (replicateParallel n c5)
  {-# INLINABLE replicateParallel #-}
  genericReplicateParallel n (c1, c2, c3, c4, c5) =
      strictTuple5 (genericReplicateParallel n c1) (genericReplicateParallel n c2) (genericReplicateParallel n c3) (genericReplicateParallel n c4) (genericReplicateParallel n c5)
  {-# INLINABLE genericReplicateParallel #-}
