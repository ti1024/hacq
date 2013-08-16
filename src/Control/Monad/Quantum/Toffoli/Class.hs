{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Quantum.Toffoli.Class (
    MonadToffoli(..),
    applyXCC, applyZCC, destructiveToffoli, destructiveToffoliBit, reduceCtrls) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (lift)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

import Control.Monad.Memo.Null
import Control.Monad.Quantum.Base.Class
import Util ((!))

class MonadQuantumBase w m => MonadToffoli w m | m -> w where

  -- | @applyXCC w a b@ applies NOT to @w@ conditioned on @a@ and @b@ both being |1>.
  --
  -- The three wires must be distinct.
  rawApplyXCC :: w -> w -> Bool -> w -> Bool -> m ()

  -- | @applyZCC a b c@ applies controlled-controlled Pauli-Z.
  --
  -- The three wires must be distinct.
  rawApplyZCC :: w -> Bool -> w -> Bool -> w -> Bool -> m ()

  -- | @destructiveToffoli w a b@ applies αX to @w@ conditioned on @a@ and @b@ both being |1>
  -- for some scalar α with |α|=1 which depends on implementations of MonadToffoli.
  --
  -- The three wires must be distinct.
  --
  -- Reasonable choices for α are 1 and ±i.
  -- If α=1, @destructiveToffoli@ becomes identical to `applyXCC`.
  -- This choice is used in `Control.Monad.Quantum.Base.CircuitBuilder`.
  -- If α=±i, @destructiveToffoli@ can be implemented with only 4 T gates,
  -- as discussed in the TORQUE team meeting on July 19, 2012.
  -- This choice is used in `Control.Monad.Quantum.Toffoli.ToStandard`.
  rawDestructiveToffoli :: w -> w -> Bool -> w -> Bool -> m ()

-- | @applyXCC w a b@ applies NOT to @w@ conditioned on @a@ and @b@ both being |1>.
--
-- The three wires must be distinct.
applyXCC :: MonadToffoli w m => w -> Bit w -> Bit w -> m ()
applyXCC _ (BitConst False) _ = return ()
applyXCC w (BitConst True) b = cnotWire w b
applyXCC _ _ (BitConst False) = return ()
applyXCC w a (BitConst True) = cnotWire w a
applyXCC w (BitWire nega a) (BitWire negb b) = rawApplyXCC w a nega b negb
{-# INLINABLE applyXCC #-}

-- | @applyZCC a b c@ applies controlled-controlled Pauli-Z.
--
-- The three wires must be distinct.
applyZCC :: MonadToffoli w m => Bit w -> Bit w -> Bit w -> m ()
applyZCC (BitConst False) _ _ = return ()
applyZCC (BitConst True) b c = applyZC b c
applyZCC _ (BitConst False) _ = return ()
applyZCC a (BitConst True) c = applyZC a c
applyZCC _ _ (BitConst False) = return ()
applyZCC a b (BitConst True) = applyZC a b
applyZCC (BitWire nega a) (BitWire negb b) (BitWire negc c) = rawApplyZCC a nega b negb c negc
{-# INLINABLE applyZCC #-}

-- | @destructiveToffoli w a b@ applies αX to @w@ conditioned on @a@ and @b@ both being |1>
-- for some scalar α with |α|=1 which depends on whether inputs are BitWire or BitConst
-- and on which implementation of MonadToffoli to use.
--
-- The three wires must be distinct.
--
-- Reasonable choices for α are 1 and ±i.
-- If α=1, @destructiveToffoli@ becomes identical to `applyXCC`.
-- This choice is used in `Control.Monad.Quantum.Base.CircuitBuilder`.
-- If α=±i, @destructiveToffoli@ can be implemented with only 4 T gates,
-- as discussed in the TORQUE team meeting on July 19, 2012.
-- This choice is used in `Control.Monad.Quantum.Toffoli.ToStandard`.
destructiveToffoli :: MonadToffoli w m => w -> Bit w -> Bit w -> m ()
destructiveToffoli _ (BitConst False) _ = return ()
destructiveToffoli w (BitConst True) b = cnotWire w b
destructiveToffoli _ _ (BitConst False) = return ()
destructiveToffoli w a (BitConst True) = cnotWire w a
destructiveToffoli w (BitWire nega a) (BitWire negb b) = rawDestructiveToffoli w a nega b negb
{-# INLINABLE destructiveToffoli #-}

-- |@destructiveToffoliBit r a b@ returns a `Bit` representing @r XOR (a AND b)@,
-- possibly consuming @r@.  If all @r@, @a@, and @b@ are wires, then it uses `destructiveToffoli`.
--
-- If @r@ is a wire, then this wire is returned.
-- Therefore, @r@ should be considered as “consumed”
-- while the bit returned by this function is being used.

destructiveToffoliBit :: MonadToffoli w m => Bit w -> Bit w -> Bit w -> m (Bit w)
destructiveToffoliBit (BitConst r) (BitConst a) (BitConst b) =
    return $ BitConst (r /= (a && b))
destructiveToffoliBit r a b = withoutCtrls $ do
    r' <- bitToWire r
    destructiveToffoli r' a b
    return $ bit r'
{-# INLINABLE destructiveToffoliBit #-}

reduceCtrls :: MonadToffoli w m => Int -> Seq w -> m (Seq w)
reduceCtrls n = go
  where
    go ctrls
      | m <= n = return ctrls
      | otherwise =
          go =<< do
            ctrls' <- forParM (Seq.fromList [0 .. m `div` 2 - 1]) $ \i -> do
              let a = ctrls ! (i * 2)
                  b = ctrls ! (i * 2 + 1)
              w <- ancilla
              rawDestructiveToffoli w a False b False
              return w
            if even m then
              return ctrls'
            else
              return (ctrls' |> (ctrls ! (m - 1)))
      where
        m = Seq.length ctrls

-- Instance for ReaderT
-- Requires UndecidableInstances.

instance MonadToffoli w m => MonadToffoli w (ReaderT r m) where
  rawApplyXCC a b negb c negc = lift $ rawApplyXCC a b negb c negc
  rawApplyZCC a nega b negb c negc = lift $ rawApplyZCC a nega b negb c negc
  rawDestructiveToffoli a b negb c negc = lift $ rawDestructiveToffoli a b negb c negc

-- Instance for MemoNullT
-- Requires UndecidableInstances.

instance MonadToffoli w m => MonadToffoli w (MemoNullT k m) where
  rawApplyXCC a b negb c negc = lift $ rawApplyXCC a b negb c negc
  rawApplyZCC a nega b negb c negc = lift $ rawApplyZCC a nega b negb c negc
  rawDestructiveToffoli a b negb c negc = lift $ rawDestructiveToffoli a b negb c negc
