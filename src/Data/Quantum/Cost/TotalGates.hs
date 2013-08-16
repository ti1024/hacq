{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE DeriveDataTypeable #-}

module Data.Quantum.Cost.TotalGates (TotalGates(..), getCount, showsTotalGates, showTotalGates) where

import Data.Function (on)
import Data.Functor ((<$>))
import qualified Data.List as List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Typeable

import Data.Quantum.Cost.Class
import Data.Quantum.CountKey (GateKey)
import Util (showsIntegerWithCommas)

newtype TotalGates = TotalGates {
  getTotalGates :: HashMap GateKey Integer
} deriving (Show, Typeable)

instance Cost TotalGates where
  empty = TotalGates HashMap.empty
  newWire = TotalGates HashMap.empty
  gate key = TotalGates $ HashMap.singleton key 1
  sequential (TotalGates c1) (TotalGates c2) = TotalGates $ HashMap.unionWith (+) c1 c2
  parallel (TotalGates c1) (TotalGates c2) = TotalGates $ HashMap.unionWith (+) c1 c2
  with (TotalGates c1) (TotalGates c2) = TotalGates $ HashMap.unionWith (+) ((2 *) <$> c1) c2
  adjoint c = c
  genericReplicateSequential n cost | n `seq` cost `seq` False = undefined
  genericReplicateSequential 0 _ = empty
  genericReplicateSequential n (TotalGates c) = TotalGates $ (nI *) <$> c
    where
      nI = toInteger n
  genericReplicateParallel n cost | n `seq` cost `seq` False = undefined
  genericReplicateParallel 0 _ = empty
  genericReplicateParallel n (TotalGates c) = TotalGates $ (nI *) <$> c
    where
      nI = toInteger n

getCount :: GateKey -> TotalGates -> Integer
getCount k =
    HashMap.lookupDefault 0 k . getTotalGates

showsTotalGates :: TotalGates -> ShowS
showsTotalGates (TotalGates c) s =
    foldr showEntry s . List.sortBy (compare `on` fst) $ HashMap.toList c
  where
    showEntry (k, a) =
        shows k . showString ": " . showsIntegerWithCommas a . showString "\n"

showTotalGates :: TotalGates -> String
showTotalGates c =
    showsTotalGates c ""
