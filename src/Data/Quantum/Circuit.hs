{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Quantum.Circuit (Wire(..),
    IsGate(..), Gate(..),
    IsCircuit(..),
    Circuit(..), injectCircuit) where

import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Data.Quantum.Circuit.Class
import Data.Quantum.Circuit.Invert

data Circuit g w
  = CEmpty
  | CSingleton (g w)
  | CNewWire w Bool
  | CAncilla w Bool
  | CSeq (Circuit g w) (Circuit g w)
  | CPar (Circuit g w) (Circuit g w)
  deriving (Functor, Foldable, Traversable)

injectCircuit :: IsCircuit g w c => Circuit g w -> c
injectCircuit CEmpty = empty
injectCircuit (CSingleton g) = singleton g
injectCircuit (CNewWire w inv) = newWire w inv
injectCircuit (CAncilla w inv) = ancilla w inv
injectCircuit (CSeq c1 c2) = sequential (injectCircuit c1) (injectCircuit c2)
injectCircuit (CPar c1 c2) = parallel (injectCircuit c1) (injectCircuit c2)

instance IsGate g => IsCircuit g w (Circuit g w) where

  empty = CEmpty
  singleton = CSingleton
  newWire = CNewWire
  ancilla = CAncilla

  sequential CEmpty y = y
  sequential x CEmpty = x
  sequential x y = CSeq x y

  parallel CEmpty y = y
  parallel x CEmpty = x
  parallel x y = CPar x y

instance IsGate g => IsCircuitInvert g w (Circuit g w) where
  invert CEmpty = CEmpty
  invert (CSingleton g) = CSingleton (invertGate g)
  invert (CNewWire targ inv) = CNewWire targ (not inv)
  invert (CAncilla targ inv) = CAncilla targ (not inv)
  invert (CSeq l1 l2) = CSeq (invert l2) (invert l1)
  invert (CPar l1 l2) = CPar (invert l2) (invert l1)

data CircuitShowContext = CSCNone Int | CSCSeq | CSCPar
  deriving Eq

cscPrec :: CircuitShowContext -> Int
cscPrec (CSCNone d) = d
cscPrec CSCSeq = 11
cscPrec CSCPar = 11

showsCircuit :: (Show w, Show (g w)) => CircuitShowContext -> Circuit g w -> ShowS
showsCircuit _ CEmpty =
    showString "Empty"
showsCircuit context (CSingleton g) =
    showsPrec (cscPrec context) g
showsCircuit context (CNewWire w inv) =
    showParen (cscPrec context >= 11) $
    showString "NewWire " .
    showsPrec 11 w .
    showChar ' ' .
    showsPrec 11 inv
showsCircuit context (CAncilla w inv) =
    showParen (cscPrec context >= 11) $
    showString "Ancilla " .
    showsPrec 11 w .
    showChar ' ' .
    showsPrec 11 inv
showsCircuit CSCSeq (CSeq l1 l2) =
    showsCircuit CSCSeq l1 .
    showString " " .
    showsCircuit CSCSeq l2
showsCircuit context l@(CSeq _ _) =
    showParen (cscPrec context >= cscPrec CSCSeq) $
    showString "Seq " .
    showsCircuit CSCSeq l
showsCircuit CSCPar (CPar l1 l2) =
    showsCircuit CSCPar l1 .
    showString " " .
    showsCircuit CSCPar l2
showsCircuit context l@(CPar _ _) =
    showParen (cscPrec context >= cscPrec CSCPar) $
    showString "Par " .
    showsCircuit CSCPar l

-- The following instance requires UndecidableInstances.
instance (Show w, Show (g w)) => Show (Circuit g w) where
  showsPrec = showsCircuit . CSCNone
