{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

module Control.Monad.Quantum.PhaseShift.Binary.FourierTransform (fourierTransform) where

import Control.Monad (forM_)
import Data.Functor ((<$>))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Control.Monad.Quantum.Class
import Control.Monad.Quantum.PhaseShift.Binary.Class (MonadBinaryPhaseShift, applyBinaryPhaseShift)
import Util ((!))

-- |Quantum Fourier transform.
--
-- An n-qubit state |x> in the standard basis is mapped to (1/sqrt(2^n)) sum_{y=0}^{2^n-1} e^{2 pi i xy/n} |y>,
-- where the wires are given from the least significant bit to the most significant bit.

fourierTransform :: (MonadQuantum w m, MonadBinaryPhaseShift w m) => Seq w -> m ()
fourierTransform ws
  | n == 0 =
      return ()
  | n == 1 =
      applyH (ws ! 0)
  | otherwise = compressCtrls 1 $ do
      forM_ [n - 1, n - 2 .. 0] $ \i -> do
        applyH (ws ! i)
        control (bit $ ws ! i) $ applyBinaryPhaseShift n (bit <$> Seq.take i ws)
      swapWires (Seq.take (n `div` 2) ws) (Seq.reverse $ Seq.drop ((n + 1) `div` 2) ws)
  where
    n = Seq.length ws
{-# INLINABLE fourierTransform #-}
