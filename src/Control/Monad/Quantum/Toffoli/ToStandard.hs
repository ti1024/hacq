{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Quantum.Toffoli.ToStandard where

import Control.Applicative (Applicative)
import Control.Monad (when)

import Control.Monad.Ask.Class (MonadAsk)
import Control.Monad.Memo.Class
import Control.Monad.Quantum.Base.Class
import Control.Monad.Quantum.Toffoli.Class
import Control.Monad.Quantum.CircuitBuilder.Class
import Control.Monad.Quantum.Counter.Class
import Data.Quantum.Wire

-- |An implementation of `MonadToffoli` on top of a `MonadQuantumBase`.
--
-- `applyXCC` and `applyZCC` uses the implementation of Toffoli with 7 T gates and T-depth 1,
-- presented by Peter Selinger in the IARPA QCS review meeting in July 2012.
--
-- `destructiveToffoli` uses the implementation with 4 T gates and T-depth 1,
-- as discussed in the TORQUE team meeting on July 19, 2012.
-- As a result, @destructiveToffoli w a b@ applies −iX to @w@ conditioned on @a@ and @b@ both being |1>.

newtype ToffoliToStandardT w m a = ToffoliToStandardT {
  runToffoliToStandardT :: m a
} deriving (Functor, Applicative, Monad, MonadAsk r, MonadQuantumBase w, MonadMemo k, MonadQuantumCounter w c)

deriving instance MonadCircuitBuilder g m => MonadCircuitBuilder g (ToffoliToStandardT Wire m)

instance MonadQuantumBase w m => MonadToffoli w (ToffoliToStandardT w m) where

  rawApplyXCC a b negb c negc =
      with_ (applyH a) $
        rawApplyZCC a False b negb c negc
  {-# INLINABLE rawApplyXCC #-}

  rawApplyZCC a nega b negb c negc =
      with (do
          when nega $ applyX a
          when negb $ applyX b
          when negc $ applyX c
          ab <- ancilla
          bc <- ancilla
          ca <- ancilla
          abc <- ancilla
          cnotWire ab (bit a)
          cnotWire ca (bit a)
          cnotWire abc (bit a)
          cnotWire bc (bit b)
          cnotWire ab (bit b)
          cnotWire abc (bit b)
          cnotWire ca (bit c)
          cnotWire bc (bit c)
          cnotWire abc (bit c)
          return (ab, bc, ca, abc)) $ \(ab, bc, ca, abc) ->
        parallels_ [applyT (bit a) False, applyT (bit b) False, applyT (bit c) False,
                       applyT (bit ab) True, applyT (bit bc) True, applyT (bit ca) True,
                       applyT (bit abc) False]
  {-# INLINABLE rawApplyZCC #-}

  rawDestructiveToffoli w a nega b negb =
      with (do
          when nega $ applyX a
          when negb $ applyX b
          aw <- ancilla
          bw <- ancilla
          abw <- ancilla
          applyH w
          cnotWire aw (bit a)
          cnotWire abw (bit a)
          cnotWire bw (bit b)
          cnotWire abw (bit b)
          cnotWire aw (bit w)
          cnotWire bw (bit w)
          cnotWire abw (bit w)
          return (aw, bw, abw)) $ \(aw, bw, abw) ->
        parallels_ [applyT (bit w) False, applyT (bit aw) True, applyT (bit bw) True, applyT (bit abw) False]
  {-# INLINABLE rawDestructiveToffoli #-}
