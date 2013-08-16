{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE FlexibleContexts #-}

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Foldable as Foldable
import Data.Functor ((<$>))
import Data.Sequence (Seq)
import qualified System.IO as IO
import System.Environment

import Control.Monad.Quantum.Class
import Control.Monad.Quantum.Counter
import Control.Monad.Quantum.CircuitBuilderToffoli
import Control.Monad.Quantum.CircuitBuilder
import Data.Quantum.Circuit (Wire, Gate, IsCircuit, Circuit)
import Data.Quantum.Circuit.QC (runQCBuilder)
import Data.Quantum.Cost.TotalGates (showTotalGates)
import Data.Quantum.Cost.Width (showWidth)

-- |Adder circuit.
--
-- Given lists @x@ and @y@ of the same length n, @adderList x y c@ replaces @x@ with @x+y+c@ modulo 2^n.
--
-- The circuit is based on the reversible ripple-carry adder described in Section 2 of
-- Cuccaro, Draper, Kutin, and Moulton (arXiv:quant-ph/0410184v1)
-- and the implementation of Toffoli presented by Peter Selinger.
-- They are improved for the number of T gates based on the discussion
-- in the TORQUE team meeting on July 19, 2012.

adderList :: MonadQuantum w m => [w] -> [Bit w] -> w -> Bit w -> m ()
adderList [] [] z c =
    control c $ applyX z
adderList (xlsb : x') (ylsb : y') z c = compressCtrls 1 $ do
    control ylsb $ applyX xlsb
    with (do
        c' <- cnotBit c ylsb
        destructiveToffoliBit ylsb (bit xlsb) c') $ \ylsb' ->
      adderList x' y' z ylsb'
    control c $ applyX xlsb
adderList _ _ _ _ =
    error "adderList: Lengths do not match"

adder :: MonadQuantum w m => Seq w -> Seq (Bit w) -> w -> Bit w -> m ()
adder x y = adderList (Foldable.toList x) (Foldable.toList y)

-- The execution of a Haskell program starts with a function called "main".
main :: IO ()
main = do
    args <- getArgs 
    let size = read $ head args
    let builder :: MonadQuantum w m => m ()
        builder = do
          c <- newWire
          x <- newWires size
          y <- newWires size
          z <- newWire
          adder x (bit <$> y) z (bit c) 
    -- Compute the cost of the circuit, and name the result "cost".
    let (width, totalGates) = generateCost builder ()
    -- Output the cost.
    putStr "\n=== cost ===\n\n"
    putStr $ showWidth width
    putStr $ showTotalGates totalGates
    -- Generate the circuit with Toffoli preserved, and name the result "circuit".
    let circuitToffoli :: IsCircuit Gate Wire c => c
        circuitToffoli = buildCircuitToffoli builder
    -- Output the circuit with full information (including which part is counted as parallelized).
    putStr "\n=== circuit (internal format, Toffoli preserved) ===\n\n"
    print (circuitToffoli :: Circuit Gate Wire)
    -- Output the circuit in the QC format.
    putStr "\n=== circuit (QC format, Toffoli preserved) ===\n\n"
    ByteString.putStr $ runQCBuilder IO.nativeNewline circuitToffoli
    -- Generate the circuit, and name the result "circuit".
    let circuit :: IsCircuit Gate Wire c => c
        circuit = buildCircuit builder
    -- Output the circuit with full information (including which part is counted as parallelized).
    putStr "\n=== circuit ===\n\n"
    print (circuit :: Circuit Gate Wire)
    -- Output the circuit in the QC format.
    putStr "\n=== circuit (QC format) ===\n\n"
    ByteString.putStr $ runQCBuilder IO.nativeNewline circuit
