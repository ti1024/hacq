{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE DeriveDataTypeable #-}

module Data.Quantum.Wire (Wire(..)) where

import Data.Hashable (Hashable(hashWithSalt))
import Data.Typeable (Typeable)

newtype Wire = Wire {
  getWire :: Integer
} deriving (Eq, Typeable)

instance Show Wire where
  showsPrec d (Wire x) = showsPrec d x

instance Hashable Wire where
  s `hashWithSalt` Wire idx = s `hashWithSalt` idx
