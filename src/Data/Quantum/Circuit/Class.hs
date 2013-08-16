{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Quantum.Circuit.Class (Wire(..),
    IsGate(..), Gate(..),
    IsCircuit(..)) where

import Data.Quantum.Gate

class IsGate g => IsCircuit g w c | c -> g, c -> w where
  empty :: c
  singleton :: g w -> c
  newWire :: w -> Bool -> c
  ancilla :: w -> Bool -> c
  sequential :: c -> c -> c
  parallel :: c -> c -> c
