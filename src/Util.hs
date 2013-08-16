{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

module Util (
    -- * Functions supporting `Data.List`
    mergeAscLists, disjointAscLists,
    -- * Functions supporting `Control.Monad`
    foldingForM, mapAccumLM,
    -- * Functions supporting `Data.Sequence`
    (!), foldableToSeq, takeLast, rotateSeqR, intToBinary, WrappedSeq(..),
    -- * Functions supporting `Control.Monad.Writer.Class`
    writer', stealOutput,
    -- * Functions supporting `Control.Monad.RWS`
    runRWSWithLocalState, evalRWSWithLocalState,
    -- * Functions supporting `Control.Applicative`
    replicateA_, genericReplicateA_,
    -- * Miscellaneous functions
    isqrt, insertCommas, showsWithCommas, showIntegerWithCommas, showsIntegerWithCommas,
    showsTracTableRow
) where

import Control.Applicative (Applicative, pure, (*>))
import Control.Monad ((>=>))
import Control.Monad.Reader.Class (MonadReader)
import qualified Control.Monad.Reader.Class as Reader
import Control.Monad.RWS (RWS)
import qualified Control.Monad.RWS as RWS
import Control.Monad.Writer.Class (MonadWriter)
import qualified Control.Monad.Writer.Class as Writer
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Data.Hashable (Hashable(hashWithSalt))
import qualified Data.Monoid as Monoid
import Data.Sequence (Seq, (<|), ViewR((:>)))
import qualified Data.Sequence as Seq

mergeAscLists :: Ord a => [a] -> [a] -> [a]
mergeAscLists [] ys = ys
mergeAscLists xs [] = xs
mergeAscLists xs@(x : xs') ys@(y : ys')
  | x < y = x : mergeAscLists xs' ys
  | x > y = y : mergeAscLists xs ys'
  | otherwise = x : mergeAscLists xs' ys'

disjointAscLists :: Ord a => [a] -> [a] -> Bool
disjointAscLists [] _ = True
disjointAscLists _ [] = True
disjointAscLists xs@(x : xs') ys@(y : ys')
  | x < y = disjointAscLists xs' ys
  | x > y = disjointAscLists xs ys'
  | otherwise = False

-- | Monadic fold over the elements of a list, associating to the left, i.e., from left to right.
-- @foldingForM xs f init@ is equivalent to @`Data.Foldable.foldlM` (flip f) init xs@ when @xs@ is a list.
{-# INLINABLE foldingForM #-}
foldingForM :: Monad m => [b] -> (b -> a -> m a) -> a -> m a
foldingForM xs f = foldr (>=>) return $ map f xs
-- Equivalently, foldingForM xs f init = foldM (flip f) init xs

-- | Monadic version of `Data.List.mapAccumL`.
{-# INLINABLE mapAccumLM #-}
mapAccumLM :: Monad m => (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, [y])
mapAccumLM _ acc [] =
    return (acc, [])
mapAccumLM f acc (x : xs) = do
    (acc', y) <- f acc x
    (final, ys) <- mapAccumLM f acc' xs
    return (final, y : ys)

-- | Synonym for `Seq.index`.
(!) :: Seq a -> Int -> a
(!) = Seq.index

-- | Convert a `Foldable` to a `Seq`.
{-# INLINABLE foldableToSeq #-}
foldableToSeq :: Foldable t => t a -> Seq a
foldableToSeq = Foldable.foldMap Seq.singleton

{-# RULES
"foldableToSeq/Seq" foldableToSeq = id
"foldableToSeq/list" foldableToSeq = Seq.fromList
  #-}

takeLast :: Int -> Seq a -> Seq a
takeLast k xs
  | k <= n =
      Seq.drop (n - k) xs
  | otherwise =
      error "takeLast: Not enough elements"
  where
    n = Seq.length xs

-- | Rotate a sequence to the right; that is, remove the last element from a sequence and prepend it.
rotateSeqR :: Seq a -> Seq a
rotateSeqR zs =
    case Seq.viewr zs of
      Seq.EmptyR -> Seq.empty
      zs' :> z -> z <| zs'

-- | @intToBinary n x@ evaluates to the sequence of @n@ Bool values
-- where the i-th element represents the i-th bit of @x@ counted from the least significant bit.
intToBinary :: Integral a => Int -> a -> Seq Bool
intToBinary 0 _ = Seq.empty
intToBinary len x = odd x <| intToBinary (len - 1) (x `div` 2)

newtype WrappedSeq a = WrappedSeq {
  unwrapSeq :: Seq a
} deriving Eq

instance Hashable a => Hashable (WrappedSeq a) where
  s `hashWithSalt` WrappedSeq xs = s `hashWithSalt` Foldable.toList xs

writer' :: MonadWriter w m => (a, w) -> m a
writer' aw@(_, w) = w `seq` Writer.writer aw

-- | @stealOutput m@ is an action that executes the action @m@ and adds its output to the value of the computation,
-- and cancel output itself.
{-# INLINABLE stealOutput #-}
stealOutput :: MonadWriter w m => m a -> m (a, w)
stealOutput =
    Writer.censor (const Monoid.mempty) . Writer.listen

runRWSWithLocalState :: (MonadReader r m, MonadWriter w m) => s -> RWS r w s a -> m (a, s)
runRWSWithLocalState s m = do
    r <- Reader.ask
    let (a, s', w) = RWS.runRWS m r s
    Writer.tell w
    return (a, s')

evalRWSWithLocalState :: (MonadReader r m, MonadWriter w m) => s -> RWS r w s a -> m a
evalRWSWithLocalState s m = do
    r <- Reader.ask
    let (a, w) = RWS.evalRWS m r s
    Writer.tell w
    return a

replicateA_ :: Applicative f => Int -> f a -> f ()
replicateA_ = genericReplicateA_
{-
Equivalent to:
replicateA_ n x =
    Foldable.sequenceA_ $ Seq.replicate n x
-}

genericReplicateA_ :: (Integral i, Applicative f) => i -> f a -> f ()
genericReplicateA_ n
  | n >= 0 = go n
  | otherwise =
      error "replicateA_: n must be nonnegative"
  where
    go n1 x
      | n1 == 0 =
          pure ()
      | even n1 =
          xs *> (xs *> pure ())
      | otherwise =
          x *> (xs *> (xs *> pure ()))
      where
        xs = go (n1 `div` 2) x

-- | The floor of the square root of an integer.
isqrt :: Integral a => a -> a
isqrt a = go a
  where
    go x =
        if x' >= x then
          x
        else
          go x'
      where
        x' = (x + a `div` x) `div` 2

insertCommas :: String -> String
insertCommas =
    reverse . insertCommasRev . reverse
  where
    insertCommasRev [] = []
    insertCommasRev s@[_] = s
    insertCommasRev s@[_, _] = s
    insertCommasRev s@[_, _, _] = s
    insertCommasRev (c0 : c1 : c2 : s') = c0 : c1 : c2 : ',' : insertCommasRev s'

showsWithCommas :: Show a => a -> ShowS
showsWithCommas =
    showString . insertCommas . show

-- | The decimal string representation of an integer with a comma inserted in every three digits.
showIntegerWithCommas :: Integer -> String
showIntegerWithCommas x =
    insertCommas $ show x

-- | The decimal string representation of an integer with a comma inserted in every three digits.
showsIntegerWithCommas :: Integer -> ShowS
showsIntegerWithCommas =
    showString . showIntegerWithCommas

showsTracTableRow :: [ShowS] -> ShowS
showsTracTableRow =
    foldr (\showsCell -> ((showString "||" . showsCell) .)) (showString "||\n")
