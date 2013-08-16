{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad (replicateM)
import qualified Data.ByteString.Lazy as ByteString
import qualified System.IO as IO

import Control.Monad.Quantum.Class
import Control.Monad.Quantum.Counter
import Control.Monad.Quantum.CircuitBuilder
import Data.Quantum.Circuit (Wire, Gate, IsCircuit, Circuit)
import Data.Quantum.Circuit.QC (runQCBuilder)
import Data.Quantum.Cost.TotalGates (showTotalGates)
import Data.Quantum.Cost.Width (showWidth)

main :: IO ()
main = do
    putStrLn "Multiply controlled NOT"
    let builder :: MonadQuantum w m => m ()
        builder = do
          ctrls <- replicateM 10 newWire
          targ <- newWire
          controls (map bit ctrls) $ compressCtrls 1 $ do
            applyX targ
            applyH targ
            applyX targ
    -- Compute the cost of the circuit, and name the result "cost".
    let (width, totalGates) = generateCost builder ()
    -- Output the cost.
    putStr "\n=== cost ===\n\n"
    putStr $ showWidth width
    putStr $ showTotalGates totalGates
    -- Generate the circuit, and name the result "circuit".
    let circuit :: IsCircuit Gate Wire c => c
        circuit = buildCircuit builder
    -- Output the circuit with full information (including which part is counted as parallelized).
    putStr "\n=== circuit ===\n\n"
    print (circuit :: Circuit Gate Wire)
    -- Output the circuit in the QC format.
    putStr "\n=== circuit (QC format) ===\n\n"
    ByteString.putStr $ runQCBuilder IO.nativeNewline circuit
