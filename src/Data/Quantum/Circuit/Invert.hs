{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Quantum.Circuit.Invert (IsCircuitInvert(..), CircuitInvert, runCircuitInvert) where

import Control.Applicative (Applicative, liftA2)
import Control.Monad.Reader (Reader, reader, runReader)
import qualified Control.Monad.Reader as Reader

import Data.Quantum.Circuit.Class

class IsCircuit g w c => IsCircuitInvert g w c | c -> g, c -> w where
  invert :: c -> c

newtype CircuitInvert a = CircuitInvert {
  _unwrapCircuitInvert :: Reader Bool a
} deriving (Functor, Applicative, Monad)

runCircuitInvert :: CircuitInvert a -> a
runCircuitInvert (CircuitInvert m) =
    runReader m False

instance IsCircuit g w c => IsCircuit g w (CircuitInvert c) where

  empty = return empty
  {-# INLINABLE empty #-}

  singleton g = CircuitInvert $ reader $ \inv ->
      singleton $ if inv then invertGate g else g
  {-# INLINABLE singleton #-}

  newWire w inv = CircuitInvert $ reader $ \inv' ->
      newWire w (inv /= inv')
  {-# INLINABLE newWire #-}
  ancilla w inv = CircuitInvert $ reader $ \inv' ->
      ancilla w (inv /= inv')
  {-# INLINABLE ancilla #-}

  sequential (CircuitInvert m1) (CircuitInvert m2) = CircuitInvert $ do
      inv <- Reader.ask
      if inv then
        liftA2 sequential m2 m1
      else
        liftA2 sequential m1 m2
  {-# INLINABLE sequential #-}

  parallel (CircuitInvert m1) (CircuitInvert m2) = CircuitInvert $
      liftA2 parallel m1 m2
  {-# INLINABLE parallel #-}

instance IsCircuit g w c => IsCircuitInvert g w (CircuitInvert c) where
  invert (CircuitInvert m) = CircuitInvert $ reader $ \inv ->
      runReader m $! not inv
  {-# INLINABLE invert #-}
