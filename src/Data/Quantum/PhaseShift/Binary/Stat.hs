{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE DeriveDataTypeable #-}

module Data.Quantum.PhaseShift.Binary.Stat (
    BinaryPhaseShiftStat(..), IsBinaryPhaseShiftStat(..)) where

import Data.Typeable

import Data.Quantum.Cost.Class

data BinaryPhaseShiftStat = BinaryPhaseShiftStat {
  maxBinaryPhaseShiftDegree :: !Int,
  getParallelBinaryPhaseShifts :: !Integer
} deriving (Show, Typeable)

instance Cost BinaryPhaseShiftStat where
  empty = BinaryPhaseShiftStat 0 0
  newWire = BinaryPhaseShiftStat 0 0
  gate _ = BinaryPhaseShiftStat 0 0
  sequential (BinaryPhaseShiftStat deg1 par1) (BinaryPhaseShiftStat deg2 par2) =
      BinaryPhaseShiftStat (max deg1 deg2) (max par1 par2)
  parallel (BinaryPhaseShiftStat deg1 par1) (BinaryPhaseShiftStat deg2 par2) =
      BinaryPhaseShiftStat (max deg1 deg2) (par1 + par2)
  with (BinaryPhaseShiftStat deg1 par1) (BinaryPhaseShiftStat deg2 par2) =
      BinaryPhaseShiftStat (max deg1 deg2) (max par1 par2)
  adjoint c = c
  genericReplicateSequential 0 _ = empty
  genericReplicateSequential _ c = c
  genericReplicateParallel 0 _ = empty
  genericReplicateParallel n (BinaryPhaseShiftStat deg par) =
      BinaryPhaseShiftStat deg (nI * par)
    where
      nI = toInteger n

class IsBinaryPhaseShiftStat c where
  singletonBinaryPhaseShiftStat :: Int -> c

instance IsBinaryPhaseShiftStat BinaryPhaseShiftStat where
  singletonBinaryPhaseShiftStat deg = BinaryPhaseShiftStat deg 1
