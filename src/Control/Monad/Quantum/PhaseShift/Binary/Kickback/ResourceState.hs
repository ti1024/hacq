{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

module Control.Monad.Quantum.PhaseShift.Binary.Kickback.ResourceState (
    prepareResourceState, prepareResourceStates, cloneResourceStates,
    chooseResourceStatePrecision) where

import Data.Function (on)
import Data.Functor ((<$>))
import Data.Ratio
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq

import Control.Monad.Quantum.Class
import Control.Monad.Quantum.PhaseShift.Class
import Control.Monad.Quantum.Adder (subtractor)

-- In resource state, index 0 means LSB in the adder.

prepareResourceStates :: (MonadQuantum w m, MonadPhaseShift w angle m) => Int -> Int -> Integer -> m (Seq (Seq w))
prepareResourceStates _ _ 0 =
    return Seq.empty
prepareResourceStates k l n = do
    ws <- (prepareResourceState :: MonadPhaseShift w angle m => Int -> Int -> m (Seq w)) k l
    cloneResourceStates n ws

prepareResourceState :: MonadPhaseShift w angle m => Int -> Int -> m (Seq w)
prepareResourceState k l =
    forParM (Seq.fromList [0 .. k - 1]) $ \i -> do
      w <- ancilla
      applyH w
      case k - i of
        1 ->
          applyZ (bit w)
        2 ->
          applyS (bit w) False
        3 ->
          applyT (bit w) False
        j | j <= l ->
          applyPhaseShift (1 / 2 ^ j) (bit w)
        _ ->
          return ()
      return w

cloneResourceState :: MonadQuantum w m => Seq w -> m (Seq w)
cloneResourceState res =
    if k <= 3 then
      parallels $ Seq.fromList $ drop (3 - k) [prepare3, prepare2, prepare1]
    else
      uncurry (><) <$> parallel prepareRest (parallels (Seq.fromList [prepare3, prepare2, prepare1]))
  where
    k = Seq.length res
    -- Prepare the MSB of the resource state
    prepare1 = do
        w <- ancilla
        applyH w
        applyZ (bit w)
        return w
    -- Prepare the second bit from the MSB of the resource state
    prepare2 = do
        w <- ancilla
        applyH w
        applyS (bit w) False
        return w
    -- Prepare the third bit from the MSB of the resource state
    prepare3 = do
        w <- ancilla
        applyH w
        applyT (bit w) False
        return w
    prepareRest = do
        ws <- ancillae (k - 3)
        mapParM_ applyH ws
        subtractor res (bit <$> ws) (BitConst False)
        return ws

cloneResourceStates :: MonadQuantum w m => Integer -> Seq w -> m (Seq (Seq w))
cloneResourceStates 0 _ =
    return Seq.empty
cloneResourceStates 1 res =
    return $ Seq.singleton res
cloneResourceStates n res = do
    res' <- cloneResourceState res
    uncurry (><) <$> parallel (cloneResourceStates n' res) (cloneResourceStates (n - n') res')
  where
    n' = n `div` 2

-- |Given the degree k of binary phase shifts and the error ε allowed in generation of a resource state,
-- return some reasonable choice for precision l of the resource state
-- together with the list of rationals r such that
-- the phase shift diag [1, e^{2πri}] is used in generation of the resource state
-- and the error allowed in each of these approximation sequences.
-- The errors are in the trace distance (Schatten 1-norm).
--
-- Currently, this function chooses the precision l of the resource state
-- according to the following case analysis.
--
-- * If k≤3, then choose l=k.
-- * Otherwise, if setting l=3 is allowed (2π/8−2π/2^k ≤ ε), then choose l=3.
-- * Otherwise, choose l that maximizes the error permitted in each approximate sequence
--   (i.e., (ε−2π/2^l+2π/2^k)/(l−3)) among 4≤l≤k.
chooseResourceStatePrecision :: Int -> Double -> (Int, [Rational], Double)
chooseResourceStatePrecision k eps =
    if k <= 3 then
      (k, [], 0)
    else if 1 / 4 - 1 / 2 ^ (k - 1) <= eps / pi then
      (3, [], 0)
    else
      (l, map (\i -> 1 % 2 ^ i) [4 .. l], err)
  where
    (l, err) =
        findMaximizerBy (compare `on` snd) $ map (\i -> (i, errorPerApproxSequence i)) [4 .. k]
    errorPerApproxSequence i
      | i == k =
          eps / fromIntegral (i - 3)
      | otherwise =
          (eps + pi / 2 ^ (k - 1) - pi / 2 ^ (i - 1)) / fromIntegral (i - 3)

-- |Given a nonempty list of pairs of key and value where the sequence of keys has a unique local maxima,
-- find the pair containing the maximum key.
findMaximizerBy :: (a -> a -> Ordering) -> [a] -> a
findMaximizerBy _ [] = error "findMaximizerBy: Empty list"
findMaximizerBy cmp (hd : tl) = go hd tl
  where
    go x (y : zs) | cmp x y /= GT = go y zs
    go x _ = x
