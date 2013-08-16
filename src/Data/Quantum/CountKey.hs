{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

module Data.Quantum.CountKey (GateKey(..)) where

import Data.Hashable (Hashable, hashWithSalt)

data GateKey
  = KGateH
  | KGateXC
  | KGateS
  | KGateT
  deriving (Eq, Ord, Show)

instance Hashable GateKey where
  s `hashWithSalt` KGateH = s `hashWithSalt` (0 :: Int)
  s `hashWithSalt` KGateXC = s `hashWithSalt` (1 :: Int)
  s `hashWithSalt` KGateS = s `hashWithSalt` (2 :: Int)
  s `hashWithSalt` KGateT = s `hashWithSalt` (3 :: Int)
