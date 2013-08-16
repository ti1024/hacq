{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad (void)
import qualified Data.ByteString.Lazy as ByteString
import Data.Functor ((<$>))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified System.IO as IO

import Control.Monad.Quantum.Class
import Control.Monad.Quantum.Counter
import Control.Monad.Quantum.CircuitBuilder
import Control.Monad.Quantum.CircuitBuilderToffoli
import Data.Quantum.Circuit (Wire, Gate, IsCircuit, Circuit)
import Data.Quantum.Circuit.QC (runQCBuilder)
import Data.Quantum.Cost.TotalGates (showTotalGates)
import Data.Quantum.Cost.Width (showWidth)

uniqueSolutionOracle :: MonadQuantum w m => Seq Bool -> Seq (Bit w) -> m ()
uniqueSolutionOracle solution ws =
    controls (Seq.zipWith (\w s -> if s then w else negateBit w) ws solution) negateGlobalPhase

grover :: MonadQuantum w m => Int -> Integer -> (Seq (Bit w) -> m ()) -> m (Seq w)
grover n m oracle = do
    xs <- ancillae n
    mapParM_ applyH xs
    genericReplicateQ_ t $ do
      oracle (bit <$> xs)
      with_ (mapParM_ applyH xs) $
        controls ((\x -> negateBit $ bit x) <$> xs) negateGlobalPhase
    return xs
  where
    theta :: Double
    theta = asin $ 2 * sqrt (fromInteger (m * (2 ^ n - m))) / fromInteger (2 ^ n)
    t :: Integer
    t = round (pi / 2 / theta)

groverStat :: Int -> Integer -> (Integer, Double)
groverStat n m =
    (t, errorProbability)
  where
    theta :: Double
    theta = asin $ 2 * sqrt (fromInteger (m * (2 ^ n - m))) / fromInteger (2 ^ n)
    t :: Integer
    t = round (pi / 2 / theta)
    errorProbability = 1 - sin (theta * fromInteger t)

main :: IO ()
main = do
    putStrLn "Grover\'s algorithm"
    let solution = Seq.fromList [True, False, True, True, False]
        n = Seq.length solution
        (iterations, errorProbability) = groverStat n 1
    putStrLn $ "n = " ++ show n ++ ", m = 1"
    putStrLn $ "iteration count: " ++ show iterations
    putStrLn $ "error probability: " ++ show errorProbability
    let builder :: MonadQuantum w m => m ()
        builder =
          void $ grover n 1 (uniqueSolutionOracle solution)
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
