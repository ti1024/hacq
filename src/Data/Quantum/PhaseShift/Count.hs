{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

module Data.Quantum.PhaseShift.Count (
    PhaseShiftCount(..), IsPhaseShiftCount(..), showsPhaseShiftCount, showPhaseShiftCount) where

import Data.Quantum.Cost.Class
import Util (showsIntegerWithCommas)

newtype PhaseShiftCount = PhaseShiftCount {
  getPhaseShiftCount :: Integer
} deriving Show

instance Cost PhaseShiftCount where
  empty = PhaseShiftCount 0
  newWire = PhaseShiftCount 0
  gate _ = PhaseShiftCount 0
  sequential (PhaseShiftCount c1) (PhaseShiftCount c2) = PhaseShiftCount $ c1 + c2
  parallel (PhaseShiftCount c1) (PhaseShiftCount c2) = PhaseShiftCount $ c1 + c2
  with (PhaseShiftCount c1) (PhaseShiftCount c2) = PhaseShiftCount $ 2 * c1 + c2
  adjoint c = c
  genericReplicateSequential n (PhaseShiftCount c) = PhaseShiftCount $ nI * c
    where
      nI = toInteger n
  genericReplicateParallel n (PhaseShiftCount c) = PhaseShiftCount $ nI * c
    where
      nI = toInteger n

class IsPhaseShiftCount c where
  singletonPhaseShiftCount :: c

instance IsPhaseShiftCount PhaseShiftCount where
  singletonPhaseShiftCount = PhaseShiftCount 1

showsPhaseShiftCount :: PhaseShiftCount -> ShowS
showsPhaseShiftCount (PhaseShiftCount n) =
    showString "Phase shift: " . showsIntegerWithCommas n

showPhaseShiftCount :: PhaseShiftCount -> String
showPhaseShiftCount c =
    showsPhaseShiftCount c ""
