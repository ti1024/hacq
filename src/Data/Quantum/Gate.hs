{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.Quantum.Gate (Wire(..),
    IsGate(..), Gate(..)) where

import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.Typeable (Typeable)

import Data.Quantum.Wire (Wire(..))

class Traversable g => IsGate g where
  gateX :: w -> g w
  gateXC :: w -> w -> Bool -> g w
  gateXCC :: w -> w -> Bool -> w -> Bool -> g w
  gateH :: w -> g w
  gateY :: w -> g w -- Y|0> = i|1>, Y|1> = -i|0>
  gateZ :: w -> g w
  gateS :: w -> Bool -> g w -- gateS w False = diag[1, i], gateS w True = diag[1, -i]
  gateT :: w -> Bool -> g w -- gateT w False = diag[1, e^(i pi/4)], gateT w True = diag[1, e^(-i pi/4)]
  invertGate :: g w -> g w

data Gate w
  = GateX !w
  | GateXC !w !w !Bool
  | GateXCC !w !w !Bool !w !Bool
  | GateH !w
  | GateY !w
  | GateZ !w
  | GateS !w !Bool
  | GateT !w !Bool
  deriving (Eq, Show, Typeable, Functor, Foldable, Traversable)

instance IsGate Gate where
  gateX = GateX
  gateXC = GateXC
  gateXCC = GateXCC
  gateH = GateH
  gateY = GateY
  gateZ = GateZ
  gateS = GateS
  gateT = GateT
  invertGate (GateS targ inv) = GateS targ (not inv)
  invertGate (GateT targ inv) = GateT targ (not inv)
  invertGate e = e
