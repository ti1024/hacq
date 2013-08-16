{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

module Data.Quantum.ApproxSequence.Count (
    ApproxSequenceCount(..), IsApproxSequenceCount(..), showsApproxSequenceCount, showApproxSequenceCount) where

import Data.Quantum.Cost.Class
import Util (showsIntegerWithCommas)

newtype ApproxSequenceCount = ApproxSequenceCount {
  getApproxSequenceCount :: Integer
} deriving Show

instance Cost ApproxSequenceCount where
  empty = ApproxSequenceCount 0
  newWire = ApproxSequenceCount 0
  gate _ = ApproxSequenceCount 0
  sequential (ApproxSequenceCount c1) (ApproxSequenceCount c2) = ApproxSequenceCount $ c1 + c2
  parallel (ApproxSequenceCount c1) (ApproxSequenceCount c2) = ApproxSequenceCount $ c1 + c2
  with (ApproxSequenceCount c1) (ApproxSequenceCount c2) = ApproxSequenceCount $ 2 * c1 + c2
  adjoint c = c
  genericReplicateSequential n (ApproxSequenceCount c) = ApproxSequenceCount $ nI * c
    where
      nI = toInteger n
  genericReplicateParallel n (ApproxSequenceCount c) = ApproxSequenceCount $ nI * c
    where
      nI = toInteger n

class IsApproxSequenceCount c where
  singletonApproxSequenceCount :: c

instance IsApproxSequenceCount ApproxSequenceCount where
  singletonApproxSequenceCount = ApproxSequenceCount 1

showsApproxSequenceCount :: ApproxSequenceCount -> ShowS
showsApproxSequenceCount (ApproxSequenceCount n) =
    showString "Phase shift: " . showsIntegerWithCommas n

showApproxSequenceCount :: ApproxSequenceCount -> String
showApproxSequenceCount c =
    showsApproxSequenceCount c ""
