{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

module Data.Quantum.Cost.TotalCalls (TotalCalls(..), singletonTotalCalls, showsTotalCalls, showTotalCalls) where

import Data.Function (on)
import Data.Functor ((<$>))
import qualified Data.List as List
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.Quantum.Cost.Class
import Util (showsIntegerWithCommas)

newtype TotalCalls k = TotalCalls {
  getTotalCalls :: HashMap k Integer
} deriving Show

instance (Eq k, Hashable k) => Cost (TotalCalls k) where
  empty = TotalCalls HashMap.empty
  newWire = TotalCalls HashMap.empty
  gate _ = TotalCalls HashMap.empty
  sequential (TotalCalls c1) (TotalCalls c2) = TotalCalls $ HashMap.unionWith (+) c1 c2
  {-# INLINABLE sequential #-}
  parallel (TotalCalls c1) (TotalCalls c2) = TotalCalls $ HashMap.unionWith (+) c1 c2
  {-# INLINABLE parallel #-}
  with (TotalCalls c1) (TotalCalls c2) = TotalCalls $ HashMap.unionWith (+) ((2 *) <$> c1) c2
  {-# INLINABLE with #-}
  adjoint c = c
  genericReplicateSequential n cost | n `seq` cost `seq` False = undefined
  genericReplicateSequential 0 _ = empty
  genericReplicateSequential n (TotalCalls c) = TotalCalls $ (nI *) <$> c
    where
      nI = toInteger n
  genericReplicateParallel n cost | n `seq` cost `seq` False = undefined
  genericReplicateParallel 0 _ = empty
  genericReplicateParallel n (TotalCalls c) = TotalCalls $ (nI *) <$> c
    where
      nI = toInteger n

singletonTotalCalls :: Hashable k => k -> TotalCalls k
singletonTotalCalls k =
    TotalCalls $ HashMap.singleton k 1
{-# INLINABLE singletonTotalCalls #-}

showsTotalCalls :: (Show k, Ord k) => TotalCalls k -> ShowS
showsTotalCalls (TotalCalls c) s =
    foldr showEntry s . List.sortBy (compare `on` fst) $ HashMap.toList c
  where
    showEntry (k, a) =
        shows k . showString ": " . showsIntegerWithCommas a . showString "\n"

showTotalCalls :: (Show k, Ord k) => TotalCalls k -> String
showTotalCalls c =
    showsTotalCalls c ""
