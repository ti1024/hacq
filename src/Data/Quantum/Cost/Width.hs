{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE DeriveDataTypeable #-}

module Data.Quantum.Cost.Width (Width(..), showsWidth, showWidth, unsafeAdjoint) where

import Data.Typeable

import Data.Quantum.Cost.Class
import Data.Quantum.CountKey (GateKey(KGateT))
import Util (showsIntegerWithCommas)

-- |The main cost of a quantum circuit.
--
-- A quantum circuit consists of new-wire gates, elementary unitary gates (Clifford and T),
-- and the deallocation gates which are represented implicitly by the combination of `with` and new-wire gates.
-- A wire is said to be /in use/ if it has been allocated but has not been deallocated.
--
-- From the viewpoint of a quantum circuit, a wire is in one of the following four states:
--
-- * Old: In use at the beginning of the circuit.
--   Note that this implies that it is still in use at the end of the circuit.
-- * New: Allocated during the circuit execution and still in use at the end of the circuit execution.
-- * Ancillary: Allocated and deallocated during the circuit execution.
-- * Nonexistent: Neither in use at the beginning of the circuit nor allocated during the circuit execution.
--
-- A circuit is /responsible/ for a wire if it allocates the wire;
-- in other words, a circuit is responsible for its new wires and its ancillary wires.
-- The /width/ of the circuit is the maximum number of the wires in use for which it is responsible
-- at any moment of the circuit execution.
-- Note that old wires are not counted when determining the width.
--
-- The /T-lifetime/ of a wire is the time, measured in T-depth, for which the wire is in use during the circuit execution;
-- in other words, we start counting the T-lifetime at the beginning of circuit execution or at the allocation of the wire,
-- whichever occurs later, and we stop counting the T-lifetime at the end of circuit execution or at the deallocation
-- of the wire, whichever occurs earlier.
-- The /T-area/ of a circuit is the sum of the T-lifetimes of the wires for which it is responsible.
--
-- About implementation of `parallel` and similar methods in `Cost`:
-- For the purpose of calculating the width, we do not assume anything about the alignment
-- of the two circuits, and just use the sum of the width of the two circuits.
-- For the purpose of calculating the T-area, the two circuits are assumed to be aligned at the end of execution.

data Width = Width {
  -- |The number of the new wires of the circuit.
  countNewWires :: !Integer,
  -- |The width of the circuit.
  countWidth :: !Integer,
  -- |The T-depth of the circuit.
  countTDepth :: !Integer,
  -- |The T-area of the circuit.
  countTArea :: !Integer
} deriving (Show, Typeable)

instance Cost Width where
  empty =
      Width {
        countNewWires = 0,
        countWidth = 0,
        countTDepth = 0,
        countTArea = 0
      }
  newWire =
      Width {
        countNewWires = 1,
        countWidth = 1,
        countTDepth = 0,
        countTArea = 0
      }
  gate KGateT =
      Width {
        countNewWires = 0,
        countWidth = 0,
        countTDepth = 1,
        countTArea = 0
          -- The T-area of a T gate is 0, not 1, because the T gate is not responsible for the wire
          -- to which it is applied.
      }
  gate _ =
      Width {
        countNewWires = 0,
        countWidth = 0,
        countTDepth = 0,
        countTArea = 0
      }
  sequential cost1 cost2 =
      Width {
        countNewWires = countNewWires cost1 + countNewWires cost2,
        countWidth = max (countWidth cost1) (countNewWires cost1 + countWidth cost2),
        countTDepth = countTDepth cost1 + countTDepth cost2,
        countTArea = countTArea cost1 + countTArea cost2 + countNewWires cost1 * countTDepth cost2
      }
  parallel cost1 cost2 =
      Width {
        countNewWires = countNewWires cost1 + countNewWires cost2,
        countWidth = countWidth cost1 + countWidth cost2,
        countTDepth = max (countTDepth cost1) (countTDepth cost2),
        countTArea = countTArea cost1 + countTArea cost2
      }
  with prepare body =
      Width {
        countNewWires = countNewWires body,
        countWidth = max (countNewWires prepare + countWidth body) (countWidth prepare + countNewWires body),
        countTDepth = 2 * countTDepth prepare + countTDepth body,
        countTArea = 2 * countTArea prepare + countTArea body + countNewWires prepare * countTDepth body + countNewWires body * countTDepth prepare
      }
  adjoint cost =
      if countNewWires cost == 0 then
        cost
      else
        error "Data.Quantum.Cost.Width.adjoint: Not invertible"
  genericReplicateSequential n cost | n `seq` cost `seq` False = undefined
  -- 0 times is a special case: countWidth is 0 when applying 0 times
  genericReplicateSequential 0 _ = empty
  genericReplicateSequential n cost =
      Width {
        countNewWires = nI * countNewWires cost,
        countWidth = (nI - 1) * countNewWires cost + countWidth cost,
        countTDepth = nI * countTDepth cost,
        countTArea = nI * countTArea cost + (nI * (nI - 1) `div` 2) * countNewWires cost * countTDepth cost
      }
    where
      nI = toInteger n
  genericReplicateParallel n cost | n `seq` cost `seq` False = undefined
  -- 0 times is a special case: countTDepth is 0 when applying 0 times
  genericReplicateParallel 0 _ = empty
  genericReplicateParallel n cost =
      Width {
        countNewWires = nI * countNewWires cost,
        countWidth = nI * countWidth cost,
        countTDepth = countTDepth cost,
        countTArea = nI * countTArea cost
      }
    where
      nI = toInteger n

unsafeAdjoint :: Width -> Width
unsafeAdjoint cost =
    Width {
      countNewWires = -countNewWires cost,
      countWidth = countWidth cost - countNewWires cost,
      countTDepth = countTDepth cost,
      countTArea = countTArea cost
    }

showsWidth :: Width -> ShowS
showsWidth cost =
    showString "Width: " . showsIntegerWithCommas (countWidth cost) . showString "\n" .
    showString "T-depth: " . showsIntegerWithCommas (countTDepth cost) . showString "\n" .
    showString "T-area: " . showsIntegerWithCommas (countTArea cost) . showString "\n"

showWidth :: Width -> String
showWidth cost =
    showsWidth cost ""
