{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE DeriveDataTypeable #-}

module Data.Void (Void) where

import Data.Hashable (Hashable(hashWithSalt))
import Data.Typeable (Typeable)

data Void
  deriving Typeable

instance Eq Void where
  _ == _ = True

instance Hashable Void where
  s `hashWithSalt` _ = s
