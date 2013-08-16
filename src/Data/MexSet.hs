{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.MexSet (MexSet, empty, singleton, fromList, fromAscList, toList, null, member, notMember, insert, delete, mex) where

import Prelude hiding (null)
import Data.FingerTree (FingerTree, Measured, (<|), (><), ViewL((:<)))
import qualified Data.FingerTree as FingerTree
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.Monoid (Monoid)
import qualified Data.Monoid as Monoid

import Util (mergeAscLists)

data Mex a = Mex {
  _getMin :: !a,      -- ^minimum integer in the tree; 0 for empty tree
  getMax :: !a,       -- ^maximum integer in the tree; 0 for empty tree
  _getNextHole :: !a  -- ^minimum integer that is at least _getMin and does not appear in the tree; 0 for empty tree
}

instance Integral a => Monoid (Mex a) where
  mempty = Mex 0 0 0
  Mex _ _ 0 `mappend` mex2 = mex2
  mex1 `mappend` Mex _ _ 0 = mex1
  Mex mi1 _ ho1 `mappend` Mex mi2 ma2 ho2 =
      Mex mi1 ma2 $ if ho1 == mi2 then ho2 else ho1

newtype Item a = Item {
  getItem :: a
} deriving (Eq, Ord, Num, Real, Enum, Integral)

instance Show a => Show (Item a) where
  showsPrec d (Item x) = showsPrec d x

instance Integral a => Measured (Mex a) (Item a) where
  measure (Item x) =
      Mex x x y
    where
      y = x + 1

newtype MexSet a = MexSet (FingerTree (Mex a) (Item a))
  deriving (Eq, Ord, Show)

empty :: Integral a => MexSet a
empty = MexSet FingerTree.empty

singleton :: Integral a => a -> MexSet a
singleton x = MexSet $ FingerTree.singleton $ Item x

instance Integral a => Monoid (MexSet a) where
  mempty = empty
  MexSet t1 `mappend` MexSet t2 =
      MexSet $ FingerTree.fromList $ mergeAscLists (Foldable.toList t1) (Foldable.toList t2)

fromList :: Integral a => [a] -> MexSet a
fromList =
    fromAscList . List.sort

fromAscList :: Integral a => [a] -> MexSet a
fromAscList =
    MexSet . FingerTree.fromList . map Item

toList :: Integral a => MexSet a -> [a]
toList (MexSet t) = map getItem $ Foldable.toList t

null :: Integral a => MexSet a -> Bool
null (MexSet t) = FingerTree.null t

member :: Integral a => a -> MexSet a -> Bool
member x (MexSet t) =
    case FingerTree.viewl (FingerTree.dropUntil ((>= x) . getMax) t) of
      Item x' :< _ | x == x' -> True
      _ -> False

notMember :: Integral a => a -> MexSet a -> Bool
notMember a = not . member a

insert :: Integral a => a -> MexSet a -> MexSet a
insert x s@(MexSet t) =
    case FingerTree.viewl r of
      Item x' :< _ | x == x' -> s
      _ -> MexSet $ l >< Item x <| r
  where
    (l, r) = FingerTree.split ((>= x) . getMax) t

delete :: Integral a => a -> MexSet a -> MexSet a
delete x s@(MexSet t) =
    case FingerTree.viewl r of
      Item x' :< r' | x == x' -> MexSet $ l >< r'
      _ -> s
  where
    (l, r) = FingerTree.split ((>= x) . getMax) t

mex :: Integral a => MexSet a -> a
mex (MexSet t) =
    if mi == 0 then
      ho
    else
      0
  where
    Mex mi _ ho = FingerTree.measure t
