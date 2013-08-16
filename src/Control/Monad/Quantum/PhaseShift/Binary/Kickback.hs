{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Quantum.PhaseShift.Binary.Kickback (
    module Control.Monad.Quantum.PhaseShift.Binary.Class,
    PhaseKickbackT(..), runPhaseKickbackT) where

import Control.Applicative (Applicative)
import Control.Monad (when, unless)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Writer.Strict (WriterT(WriterT), runWriterT)
import Data.Functor ((<$>))
import Data.Monoid (Monoid, (<>))
import qualified Data.Monoid as Monoid
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Typeable (Typeable)

import Control.Monad.Ask.Class (MonadAsk, ask)
import Control.Monad.Memo.Class
import Control.Monad.Quantum.Class
import Control.Monad.Quantum.PhaseShift.Binary.Class
import Control.Monad.Quantum.ApproxSequence.Class
import Control.Monad.Quantum.PhaseShift.Class
import qualified Control.Monad.Quantum.PhaseShift.Class as PhaseShift
import Control.Monad.Quantum.Adder (adder, subtractor)
import Util ((!), takeLast)

newtype ConsumeCount = ConsumeCount Int
  deriving Typeable

instance Monoid ConsumeCount where
  mempty = ConsumeCount 0
  ConsumeCount a `mappend` ConsumeCount b = ConsumeCount (max a b)

newtype PhaseKickbackT w angle m a = PhaseKickbackT {
  unwrapPhaseKickbackT :: ReaderT (Seq (Seq w)) (WriterT ConsumeCount m) a
} deriving (Functor, Applicative, Monad)

instance MonadTrans (PhaseKickbackT w angle) where
  lift = PhaseKickbackT . lift . lift

instance MonadMemo (k, Seq (Seq w)) m => MonadMemo k (PhaseKickbackT w angle m) where
  memoize k (PhaseKickbackT m) = PhaseKickbackT $ ReaderT $ \res -> WriterT $
      memoize (k, res) (runWriterT (runReaderT m res))

instance MonadAsk r m => MonadAsk r (PhaseKickbackT w angle m) where
  ask = lift ask
  {-# INLINABLE ask #-}

instance MonadQuantumBase w m => MonadQuantumBase w (PhaseKickbackT w angle m) where

  newWire = lift newWire
  {-# INLINABLE newWire #-}

  ancilla = lift ancilla
  {-# INLINABLE ancilla #-}

  applyX w = lift $ applyX w
  {-# INLINABLE applyX #-}

  applyY w = lift $ applyY w
  {-# INLINABLE applyY #-}

  rawApplyZ w neg = lift $ rawApplyZ w neg
  {-# INLINABLE rawApplyZ #-}

  applyH w = lift $ applyH w
  {-# INLINABLE applyH #-}

  rawApplyS w neg inv = lift $ rawApplyS w neg inv
  {-# INLINABLE rawApplyS #-}

  rawApplyT w neg inv = lift $ rawApplyT w neg inv
  {-# INLINABLE rawApplyT #-}

  negateGlobalPhase =
      lift negateGlobalPhase
  {-# INLINABLE negateGlobalPhase #-}

  rotateGlobalPhase90 =
      lift . rotateGlobalPhase90
  {-# INLINABLE rotateGlobalPhase90 #-}

  rotateGlobalPhase45 =
      lift . rotateGlobalPhase45
  {-# INLINABLE rotateGlobalPhase45 #-}

  rawApplyZC a nega b negb = lift $ rawApplyZC a nega b negb
  {-# INLINABLE rawApplyZC #-}

  rawCnotWire w w1 neg1 = lift $ rawCnotWire w w1 neg1
  {-# INLINABLE rawCnotWire #-}

  rawCnotWires ws = lift $ rawCnotWires ws
  {-# INLINABLE rawCnotWires #-}

  with prepare body = PhaseKickbackT $ ReaderT $ \r -> WriterT $
      with (runWriterT $ runReaderT (unwrapPhaseKickbackT prepare) r) $ \(a, c) -> do
        (b, c') <- runWriterT $ runReaderT (unwrapPhaseKickbackT (body a)) r
        return (b, c <> c')
  {-# INLINABLE with #-}

  bindParallel m1 m2 = PhaseKickbackT $ ReaderT $ \res -> WriterT $
      bindParallel (runWriterT $ runReaderT (unwrapPhaseKickbackT m1) res) $ \(a1, ConsumeCount c1) -> do
        (a2, ConsumeCount c2) <- runWriterT $ runReaderT (unwrapPhaseKickbackT (m2 a1)) (Seq.drop c1 res)
        return (a2, ConsumeCount $ c1 + c2)
  {-# INLINABLE bindParallel #-}

  invert m = PhaseKickbackT $ ReaderT $ \r -> WriterT $
      invert (runWriterT $ runReaderT (unwrapPhaseKickbackT m) r)
  {-# INLINABLE invert #-}

  withoutCtrls m = PhaseKickbackT $ ReaderT $ \r -> WriterT $
      withoutCtrls (runWriterT $ runReaderT (unwrapPhaseKickbackT m) r)
  {-# INLINABLE withoutCtrls #-}

instance MonadToffoli w m => MonadToffoli w (PhaseKickbackT w angle m) where
  rawApplyXCC a b negb c negc = lift $ rawApplyXCC a b negb c negc
  rawApplyZCC a nega b negb c negc = lift $ rawApplyZCC a nega b negb c negc
  rawDestructiveToffoli a b negb c negc = lift $ rawDestructiveToffoli a b negb c negc

instance MonadQuantum w m => MonadQuantum w (PhaseKickbackT w angle m) where

  handleCtrls body = PhaseKickbackT $ ReaderT $ \r -> WriterT $
      handleCtrls (runWriterT . flip runReaderT r . unwrapPhaseKickbackT . body)

  rawControl w s body = PhaseKickbackT $ ReaderT $ \r -> WriterT $
      rawControl w s (runWriterT $ flip runReaderT r $ unwrapPhaseKickbackT body)

  compressCtrls n body = PhaseKickbackT $ ReaderT $ \r -> WriterT $
      compressCtrls n (runWriterT $ flip runReaderT r $ unwrapPhaseKickbackT body)

instance MonadPhaseShift w angle m => MonadPhaseShift w angle (PhaseKickbackT w angle m) where
  applyGlobalPhase fraction = lift $ PhaseShift.applyGlobalPhase fraction
  {-# INLINABLE applyGlobalPhase #-}

instance MonadApproxSequence w m => MonadApproxSequence w (PhaseKickbackT w angle m) where
  applyOneQubitUnitary a c d w = lift $ applyOneQubitUnitary a c d w
  {-# INLINABLE applyOneQubitUnitary #-}

instance MonadQuantum w m => MonadBinaryPhaseShift w (PhaseKickbackT w angle m) where

  applyGlobalBinaryPhase k m = PhaseKickbackT $ ReaderT $ \res -> WriterT $ do
      ctrls <- askCtrls
      unless (null ctrls) $
        if m > 0 then
          subtractor (takeLast k (res ! 0)) (BitConst <$> integerToBinary m) (BitConst False)
        else
          adder (takeLast k (res ! 0)) (BitConst <$> integerToBinary (-m)) (BitConst False)
      return ((), ConsumeCount 1)
  {-# INLINABLE applyGlobalBinaryPhase #-}

  applyBinaryPhaseShift k m
    | n <= 0 =
        return ()
    | n == 1 =
        control (m ! 0) $
          applyGlobalBinaryPhase k 1
    | otherwise = compressCtrls 1 $ PhaseKickbackT $ ReaderT $ \res -> WriterT $ do
        parallels_ [
          when (k >= 1 && n >= k) $
            applyZ (m ! (k - 1)),
          when (k >= 2 && n >= k - 1) $
            applyS (m ! (k - 2)) False,
          when (k >= 3 && n >= k - 2) $
            applyT (m ! (k - 3)) False,
          when (k >= 4) $
            subtractor (takeLast k (res ! 0)) (Seq.take (k - 3) m) (BitConst False)]
        return ((), ConsumeCount $ if k >= 4 then 1 else 0)
    where
      n = min k (Seq.length m)
  {-# INLINABLE applyBinaryPhaseShift #-}

integerToBinary :: Integer -> [Bool]
integerToBinary 0 = []
integerToBinary m = odd m : integerToBinary (m `div` 2)

runPhaseKickbackT :: Functor m => PhaseKickbackT w angle m a -> Seq (Seq w) -> m a
runPhaseKickbackT m res = fst <$> runWriterT (runReaderT (unwrapPhaseKickbackT m) res)
