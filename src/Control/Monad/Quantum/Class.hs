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

module Control.Monad.Quantum.Class (
    module Control.Monad.Quantum.Base.Class,
    module Control.Monad.Quantum.Toffoli.Class,
    MonadQuantum(..),
    askCtrls, control, controls, handleMaybeCtrl, computeParity, ifThenElse) where

import Control.Monad (when)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Control.Monad.Memo.Null
import Control.Monad.Quantum.Base.Class
import Control.Monad.Quantum.Toffoli.Class

class MonadToffoli w m => MonadQuantum w m | m -> w where

  -- | Pass the current control context to a given function, and apply the resulting curcuit
  -- with the empty control context.
  handleCtrls :: ([w] -> m a) -> m a

  -- | Apply a given circuit controlled on another wire.
  -- It is positive control if the second argument is @False@,
  -- and negative control if the second argument is @True@.
  rawControl :: w -> Bool -> m a -> m a

  -- | @compressCtrls n body@ applies a circuit @body@, ensuring that the control context
  -- contains at most @n@ qubits.  @n@ must be positive.
  compressCtrls :: Int -> m a -> m a

-- | Return the current control context.
askCtrls :: MonadQuantum w m => m [w]
askCtrls =
    handleCtrls return
{-# INLINABLE askCtrls #-}

-- | Apply a given circuit controlled on another wire.
control :: MonadQuantum w m => Bit w -> m () -> m ()
control (BitConst b) m =
    when b m
control (BitWire neg w) m =
    rawControl w neg m

-- | Add zero or more controls.
controls :: (MonadQuantum w m, Foldable t) => t (Bit w) -> m () -> m ()
controls ctrls m =
    Foldable.foldr control m ctrls
{-# INLINABLE controls #-}

handleMaybeCtrl :: MonadQuantum w m => (Maybe w -> m a) -> m a
handleMaybeCtrl handler =
    compressCtrls 1 $ handleCtrls $ handler . Maybe.listToMaybe
{-# INLINABLE handleMaybeCtrl #-}

-- | @computeParity r ws@ applies NOT to @r@ controlled on the parity of @ws@ being odd.
--
-- If the control context is empty, it simply applies @n@ CNOT gates, where @n@ is the length of @ws@.
-- If the control context contains @k@ qubits, @computeParity@ uses 2n CNOT and one (k+1)-controlled NOT
-- instead of @n@ @k@-controlled NOT.
computeParity :: (MonadQuantum w m, Foldable t) => w -> t (Bit w) -> m ()
computeParity r ws =
    computeParityList r (Foldable.toList ws)
{-# INLINABLE computeParity #-}

computeParityList :: MonadQuantum w m => w -> [Bit w] -> m ()
computeParityList r ws =
    rawComputeParityList inv r [w | BitWire _ w <- ws]
  where
    inv = List.foldl' (/=) False $ map f ws
    f (BitConst b) = b
    f (BitWire neg _) = neg
{-# INLINABLE computeParityList #-}

rawComputeParityList :: MonadQuantum w m => Bool -> w -> [w] -> m ()
rawComputeParityList False _ [] =
    return ()
rawComputeParityList True w [] =
    applyX w
rawComputeParityList neg r (w : ws') = do
    ctrls <- askCtrls
    if null ctrls then do
      cnotWire r (BitWire neg w)
      Foldable.mapM_ (cnotWire r . bit) ws'
    else
      with_ (Foldable.mapM_ (cnotWire w . bit) ws') $
        rawControl w neg $ applyX r
{-# INLINABLE rawComputeParityList #-}

ifThenElse :: MonadQuantum w m => Bit w -> m () -> m () -> m ()
ifThenElse (BitConst True) t _ = t
ifThenElse (BitConst False) _ f = f
ifThenElse (BitWire False d) t f = ifThenElseSub d t f
ifThenElse (BitWire True d) t f = ifThenElseSub d f t

ifThenElseSub :: MonadQuantum w m => w -> m () -> m () -> m ()
ifThenElseSub d t f = handleMaybeCtrl $ \ctrl ->
    case ctrl of
      Nothing -> do
        control (bit d) t
        control (negateBit (bit d)) f
      Just c ->
        with ancilla $ \w ->
        with ancilla $ \cd -> do
          with_ (do
              cnotWire cd (bit c)
              cnotWire cd (bit d)
              with (do
                  cw <- ancilla
                  dw <- ancilla
                  cdw <- ancilla
                  applyH w
                  cnotWire cw (bit c)
                  cnotWire cdw (bit c)
                  cnotWire dw (bit d)
                  cnotWire cdw (bit d)
                  cnotWire cw (bit w)
                  cnotWire dw (bit w)
                  cnotWire cdw (bit w)
                  return (cw, dw, cdw)) $ \(cw, dw, cdw) -> do
                parallels_ [applyT (bit w) False, applyT (bit cw) True, applyT (bit dw) True, applyT (bit cdw) False]) $ do
            control (bit w) t
            control (bit c) $ applyX w
            applyS (bit d) False
            applyX cd
            applyS (bit cd) False
            control (bit w) f
            applyX d
          applyX d
{-# INLINE ifThenElse #-}

-- Instance for ReaderT
-- Requires UndecidableInstances.

instance MonadQuantum w m => MonadQuantum w (ReaderT r m) where

  handleCtrls body = ReaderT $ \r ->
      handleCtrls (flip runReaderT r . body)

  rawControl w s body = ReaderT $ \r ->
      rawControl w s (runReaderT body r)

  compressCtrls n body = ReaderT $ \r ->
      compressCtrls n (runReaderT body r)

-- Instance for MemoNullT
-- Requires UndecidableInstances.

instance MonadQuantum w m => MonadQuantum w (MemoNullT k m) where

  handleCtrls body = MemoNullT $
      handleCtrls (runMemoNullT . body)

  rawControl w s body = MemoNullT $
      rawControl w s (runMemoNullT body)

  compressCtrls n body = MemoNullT $
      compressCtrls n (runMemoNullT body)
