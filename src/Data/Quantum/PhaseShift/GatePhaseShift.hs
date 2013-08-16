{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.Quantum.PhaseShift.GatePhaseShift (GatePhaseShift(..), invertGatePhaseShift, IsGatePhaseShift(..)) where

import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

data GatePhaseShift w
  = GatePhaseShift w Double
  deriving (Eq, Show, Functor, Foldable, Traversable)

invertGatePhaseShift :: GatePhaseShift w -> GatePhaseShift w
invertGatePhaseShift (GatePhaseShift w fraction) =
    GatePhaseShift w (-fraction)

class IsGatePhaseShift g where
  gatePhaseShift :: w -> Double -> g w

instance IsGatePhaseShift GatePhaseShift where
  gatePhaseShift = GatePhaseShift
