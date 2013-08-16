{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

-- | Adder, subtractor, and comparator circuits.
--
-- The circuits are based on the reversible ripple-carry adder described in Section 2 of
-- Cuccaro, Draper, Kutin, and Moulton (arXiv:quant-ph/0410184v1)
-- and the implementation of Toffoli presented by Peter Selinger.
-- They are improved for the number of T gates based on the discussion
-- in the TORQUE team meeting on July 19, 2012.

module Control.Monad.Quantum.Adder (
    increment, decrement, adder, subtractor, comparator) where

import Control.Monad (unless)
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Data.Functor ((<$>))

import Control.Monad.Quantum.Class

increment :: (MonadQuantum w m, Foldable t) => t w -> m ()
increment x =
    incrementList (Foldable.toList x)
{-# INLINE increment #-}

incrementList :: MonadQuantum w m => [w] -> m ()
incrementList [] =
    return ()
incrementList [xlsb] =
    applyX xlsb
incrementList (xlsb : x') = compressCtrls 1 $ do
    control (bit xlsb) $ incrementList x'
    applyX xlsb
{-# INLINABLE incrementList #-}

decrement :: (MonadQuantum w m, Foldable t) => t w -> m ()
decrement x =
    decrementList (Foldable.toList x)
{-# INLINE decrement #-}

decrementList :: MonadQuantum w m => [w] -> m ()
decrementList [] =
    return ()
decrementList [xlsb] =
    applyX xlsb
decrementList (xlsb : x') = compressCtrls 1 $ do
    applyX xlsb
    control (bit xlsb) $ decrementList x'
{-# INLINABLE decrementList #-}

-- |Adder circuit.
--
-- @adder x y c@ replaces @x@ with @x+y+c@ modulo 2^m, where m is the length of @x@.
-- @x@ and @y@ do not have to have the same length.  If @y@ is shorter, it is treated as unsigned.
adder :: (MonadQuantum w m, Foldable t1, Foldable t2) => t1 w -> t2 (Bit w) -> Bit w -> m ()
adder x y c =
    adderList (Foldable.toList x) (Foldable.toList y) c
{-# INLINE adder #-}

adderList :: MonadQuantum w m => [w] -> [Bit w] -> Bit w -> m ()
adderList [] _ _ =
    return ()
adderList x [] c =
    control c $ incrementList x
adderList (a : x') (b : y') c =
    adder2 a b c x' y'
{-# INLINABLE adderList #-}

adder0 :: MonadQuantum w m => w -> Bool -> Bool -> [w] -> [Bit w] -> m ()
adder0 a b c x' y'
  | b == c =
      adderList x' y' (BitConst c)
  | otherwise = compressCtrls 1 $ do
      adderList x' y' (bit a)
      applyX a
{-# INLINABLE adder0 #-}

adder1 :: MonadQuantum w m => w -> Bit w -> Bool -> [w] -> [Bit w] -> m ()
adder1 a (BitConst b) c x' y' =
    adder0 a b c x' y'
adder1 a b c x' y' = compressCtrls 1 $ do
    unless (null x') $ with (do
        c' <- ancilla
        destructiveToffoli c' (xorBoolBit c $ bit a) (xorBoolBit c b)
        return $ xorBoolBit c $ bit c') $ \c' ->
      adderList x' y' c'
    control (xorBoolBit c b) $ applyX a
{-# INLINABLE adder1 #-}

adder2 :: MonadQuantum w m => w -> Bit w -> Bit w -> [w] -> [Bit w] -> m ()
adder2 a b (BitConst c) x' y' =
    adder1 a b c x' y'
adder2 a (BitConst b) c x' y' =
    adder1 a c b x' y'
adder2 a b c x' y' = compressCtrls 1 $ do
    control b $ applyX a
    unless (null x') $ with (do
        bc <- cnotBit c b
        destructiveToffoliBit b (bit a) bc) $ \c' ->
      adderList x' y' c'
    control c $ applyX a
{-# INLINABLE adder2 #-}

-- |Subtractor circuit.
--
-- @subtractor x y c@ replaces @x@ with @x-y-c@ modulo 2^m, where m is the length of @x@.
-- @x@ and @y@ do not have to have the same length.  If @y@ is shorter, it is treated as unsigned.
subtractor :: (MonadQuantum w m, Foldable t1, Foldable t2) => t1 w -> t2 (Bit w) -> Bit w -> m ()
subtractor x y c =
    subtractorList (Foldable.toList x) (Foldable.toList y) c
{-# INLINE subtractor #-}

subtractorList :: MonadQuantum w m => [w] -> [Bit w] -> Bit w -> m ()
subtractorList [] _ _ =
    return ()
subtractorList x [] c =
    control c $ decrementList x
subtractorList (a : x') (b : y') c =
    subtractor2 a b c x' y'
{-# INLINABLE subtractorList #-}

subtractor0 :: MonadQuantum w m => w -> Bool -> Bool -> [w] -> [Bit w] -> m ()
subtractor0 a b c x' y'
  | b == c =
      subtractorList x' y' (BitConst c)
  | otherwise = compressCtrls 1 $ do
      applyX a
      subtractorList x' y' (bit a)
{-# INLINABLE subtractor0 #-}

subtractor1 :: MonadQuantum w m => w -> Bit w -> Bool -> [w] -> [Bit w] -> m ()
subtractor1 a (BitConst b) c x' y' =
    subtractor0 a b c x' y'
subtractor1 a b c x' y' = compressCtrls 1 $ do
    control (xorBoolBit c b) $ applyX a
    unless (null x') $ with (do
        c' <- ancilla
        destructiveToffoli c' (xorBoolBit c $ bit a) (xorBoolBit c b)
        return $ xorBoolBit c $ bit c') $ \c' ->
      subtractorList x' y' c'
{-# INLINABLE subtractor1 #-}

subtractor2 :: MonadQuantum w m => w -> Bit w -> Bit w -> [w] -> [Bit w] -> m ()
subtractor2 a b (BitConst c) x' y' =
    subtractor1 a b c x' y'
subtractor2 a (BitConst b) c x' y' =
    subtractor1 a c b x' y'
subtractor2 a b c x' y' = compressCtrls 1 $ do
    control b $ applyX a
    unless (null x') $ with (do
        bc <- cnotBit c b
        destructiveToffoliBit b (negateBit $ bit a) bc) $ \c' ->
      subtractorList x' y' c'
    control c $ applyX a
{-# INLINABLE subtractor2 #-}

-- | @checkNonzero xs@ prepares a new wire $w$ which will be the bitwise OR of @xs@.
checkNonzero :: MonadQuantum w m => [Bit w] -> m (Bit w)
checkNonzero xs | any isConstTrue xs =
    return $ BitConst True
  where
    isConstTrue (BitConst True) = True
    isConstTrue _ = False
checkNonzero xs =
    if null xs then
      return $ BitConst False
    else
      compressCtrls 1 $ do
        r <- ancilla
        controls (map negateBit xs') $ applyX r
        return $ negateBit $ bit r
  where
    xs' = [x | x@(BitWire _ _) <- xs]
{-# INLINABLE checkNonzero #-}

-- |Comparator circuit.
--
-- @comparator x y c@ makes a new wire @r@ which will be 0 if @x>=y+c@ and 1 if @x<y+c@.
-- @x@ and @y@ do not have to have the same length.  Both @x@ and @y@ are treated as unsigned.
comparator :: (MonadQuantum w m, Foldable t1, Foldable t2) => t1 (Bit w) -> t2 (Bit w) -> Bit w -> m (Bit w)
comparator x y c =
    comparatorList (Foldable.toList x) (Foldable.toList y) c
{-# INLINE comparator #-}

comparatorList :: MonadQuantum w m => [Bit w] -> [Bit w] -> Bit w -> m (Bit w)
comparatorList [] y c =
    checkNonzero (c : y)
comparatorList _ [] (BitConst False) =
    return $ BitConst False
comparatorList x [] (BitConst True) =
    negateBit <$> checkNonzero x
comparatorList (a : x') [] c =
    with (do
        c' <- ancilla
        destructiveToffoli c' (negateBit a) c
        return $ bit c') $ \c' ->
      comparatorList x' [] c'
comparatorList (a : x') (b : y') c =
    comparator2 a b c x' y'
{-# INLINABLE comparatorList #-}

comparator0 :: MonadQuantum w m => Bit w -> Bool -> Bool -> [Bit w] -> [Bit w] -> m (Bit w)
comparator0 a b c x' y'
  | b == c =
      comparatorList x' y' (BitConst c)
  | otherwise =
      comparatorList x' y' (negateBit a)
{-# INLINABLE comparator0 #-}

comparator1 :: MonadQuantum w m => Bit w -> Bit w -> Bool -> [Bit w] -> [Bit w] -> m (Bit w)
comparator1 a (BitConst b) c x' y' =
    comparator0 a b c x' y'
comparator1 a b c x' y' =
    with (do
        c' <- ancilla
        destructiveToffoli c' (xorBoolBit c $ negateBit a) (xorBoolBit c b)
        return $ xorBoolBit c $ bit c') $ \c' ->
      comparatorList x' y' c'
{-# INLINABLE comparator1 #-}

comparator2 :: MonadQuantum w m => Bit w -> Bit w -> Bit w -> [Bit w] -> [Bit w] -> m (Bit w)
comparator2 a b (BitConst c) x' y' =
    comparator1 a b c x' y'
comparator2 a (BitConst b) c x' y' =
    comparator1 a c b x' y'
comparator2 a b c x' y' =
    with (do
        ab <- cnotBit a b
        cb <- cnotBit c b
        destructiveToffoliBit b (negateBit ab) cb) $ \c' ->
      comparatorList x' y' c'
{-# INLINABLE comparator2 #-}

xorBoolBit :: Bool -> Bit w -> Bit w
xorBoolBit False = id
xorBoolBit True = negateBit
