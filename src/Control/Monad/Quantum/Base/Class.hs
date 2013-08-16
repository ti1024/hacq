{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A class for monads which describe quantum circuits, with various gates and circuit combinators.
module Control.Monad.Quantum.Base.Class (
    Bit(..), negateBit, bit,
    MonadQuantumBase(..),
    newWires, ancillae,
    applyZ, applyS, applyT, applyZC, cnotWire, cnotWires,
    with_, parallel, parallel_,
    parallels, parallels_, mapParM, mapParM_, forParM, forParM_,
    swapWire, swapWires,
    bitToWire, cnotBit) where

import Control.Applicative (Applicative, (<*>))
import qualified Control.Applicative as Applicative
import Control.Monad (void, when)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)
import Control.Monad.Trans (lift)
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Data.Functor ((<$>))
import Data.Hashable (Hashable(hashWithSalt))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Traversable (Traversable)
import qualified Data.Traversable as Traversable
import Data.Typeable (Typeable)

import Control.Monad.Memo.Null
import Util (replicateA_, genericReplicateA_)

-- |A Bit is a Boolean constant, a wire, or the negation of a wire.
-- A circuit can accept a Bit as part of its input when the operation
-- implemented by the circuit is block-diagonal with respect to that qubit of the input.

data Bit w = BitConst !Bool | BitWire !Bool w
  deriving (Eq, Show, Functor, Foldable, Traversable, Typeable)

instance Hashable w => Hashable (Bit w) where
  s `hashWithSalt` (BitConst False) = s `hashWithSalt` (0 :: Int)
  s `hashWithSalt` (BitConst True) = s `hashWithSalt` (1 :: Int)
  s `hashWithSalt` (BitWire False w) = s `hashWithSalt` (2 :: Int) `hashWithSalt` w
  s `hashWithSalt` (BitWire True w) = s `hashWithSalt` (3 :: Int) `hashWithSalt` w

-- |The negation of a `Bit`.

negateBit :: Bit w -> Bit w
negateBit (BitConst b) = BitConst $ not b
negateBit (BitWire neg w) = BitWire (not neg) w

-- |Convert a wire to a `Bit`.

bit :: w -> Bit w
bit = BitWire False

-- | A monad which represents basic quantum circuits and their compositions.
-- @w@ is the type for wires.

class (Applicative m, Monad m) => MonadQuantumBase w m | m -> w where

  -- | Return a new qubit.
  newWire :: m w

  -- | Return a new qubit which is initialized to |0\>.
  ancilla :: m w

  -- | Apply NOT (Pauli-X) to the given wire.
  applyX :: w -> m ()

  -- | Apply the Pauli-Y gate.
  applyY :: w -> m ()

  -- | Apply the Pauli-Z gate to a wire or its negation.
  -- This is the low-level interface for an implementor of `MonadQuantumBase`;
  -- for a user, it is easier to use `applyZ`.
  rawApplyZ :: w -> Bool -> m ()

  -- | Apply the Hadamard gate.
  applyH :: w -> m ()

  -- | Apply the S gate, or its inverse if the third argument is True.
  -- This is the low-level interface for an implementor of `MonadQuantumBase`;
  -- for a user, it is easier to use `applyS`.
  rawApplyS :: w -> Bool -> Bool -> m ()

  -- | Apply the T gate, or its inverse if the third argument is True.
  -- This is the low-level interface for an implementor of `MonadQuantumBase`;
  -- for a user, it is easier to use `applyT`.
  rawApplyT :: w -> Bool -> Bool -> m ()

  -- | Negate the global phase.
  -- This is no-op if the current control context is empty.
  negateGlobalPhase :: m ()

  -- | Shift the global phase by pi/2 (if the argument is False) or -pi/2 (if the argument is True).
  -- This is no-op if the current control context is empty.
  rotateGlobalPhase90 :: Bool -> m ()

  -- | Shift the global phase by pi/4 (if the argument is False) or -pi/4 (if the argument is True).
  -- This is no-op if the current control context is empty.
  rotateGlobalPhase45 :: Bool -> m ()

  -- | Apply controlled Pauli-Z.
  --
  -- The two wires must be different.
  rawApplyZC :: w -> Bool -> w -> Bool -> m ()

  -- | @cnotWire w w1@ applies CNOT (controlled Pauli-X) with target @w@ controlled by @w1@.
  --
  -- @w@ and @w1@ must not refer to the same wire.
  rawCnotWire :: w -> w -> Bool -> m ()

  -- | Given two sequences of wires, apply CNOT to each pair of wires at the corresponding position.
  --
  -- Precondition:
  --
  -- * The given wires are all distinct.
  rawCnotWires :: [(w, w, Bool)] -> m ()

  -- | @with prepare body@ is the sequential composition of @prepare@, @body@, and the inverse of @prepare@,
  -- where @body@ takes the result of @prepare@ as its argument.
  --
  -- Precondition:
  --
  -- * The composition restores all the ancillae introduced by @prepare@ to the initial state |0\>.
  with :: m a -> (a -> m b) -> m b

  -- | Apply given two circuits, possibly in parallel.
  -- Implementations may choose to apply the given circuits sequentially.
  --
  -- Precondition:
  --
  -- * The two circuits do not refer to the same wire.
  bindParallel :: m a -> (a -> m b) -> m b

  -- | Apply the inverse of a given unitary circuit.
  --
  -- Precondition:
  --
  -- * The given circuit is unitary; that is, the circuit deallocates all new wires which it allocates.
  invert :: m a -> m a

  -- |Apply the given circuit with the empty control context.
  withoutCtrls :: m a -> m a

  -- | @replicateQ n m@ evaluates to the sequential composition of @n@ copies of circuit @m@.
  {-# INLINABLE replicateQ #-}
  replicateQ :: Int -> m a -> m (Seq a)
  replicateQ = Seq.replicateA

  -- | @replicateParallelQ n m@ evaluates to the possibly parallel composition of @n@ copies of circuit @m@.
  -- This is useful when preparing many copies of a state in new wires without referring to any old wires.
  --
  -- Precondition:
  --
  -- * No two of the @n@ copies refer to the same wire.
  --   This means that the given circuit cannot refer to any old wires unless @n@ <= 1.
  {-# INLINABLE replicateParallelQ #-}
  replicateParallelQ :: Int -> m a -> m (Seq a)
  replicateParallelQ n =
      getParallel . Seq.replicateA n . Parallel

  -- | @replicateQ_ n m@ evaluates to the sequential composition of @n@ copies of circuit @m@, and discards the result.
  {-# INLINABLE replicateQ_ #-}
  replicateQ_ :: Int -> m () -> m ()
  replicateQ_ = replicateA_

  -- | @replicateParallelQ n m@ evaluates to the possibly parallel composition of @n@ copies of circuit @m@, and discards the result.
  --
  -- Precondition:
  --
  -- * No two of the @n@ copies refer to the same wire.
  --   This means that the given circuit cannot refer to any old wires unless @n@ <= 1.
  {-# INLINABLE replicateParallelQ_ #-}
  replicateParallelQ_ :: Int -> m () -> m ()
  replicateParallelQ_ n =
      getParallel . replicateA_ n . Parallel

  -- | Generic version of `replicateQ_`.
  --
  -- Note: There is no @genericReplicateQ@ because `Seq` cannot handle size larger than `Int`.
  {-# INLINABLE genericReplicateQ_ #-}
  genericReplicateQ_ :: Integral i => i -> m () -> m ()
  genericReplicateQ_ = genericReplicateA_

  -- | Generic version of `replicateParallelQ_`.
  --
  -- Note: There is no @genericFastReplicateParallelM@ because `Seq` cannot handle size larger than `Int`.
  {-# INLINABLE genericReplicateParallelQ_ #-}
  genericReplicateParallelQ_ :: Integral i => i -> m () -> m ()
  genericReplicateParallelQ_ n =
      getParallel . genericReplicateA_ n . Parallel

-- | Return new qubits.
newWires :: MonadQuantumBase w m => Int -> m (Seq w)
newWires n
  | n >= 0 = replicateParallelQ n newWire
  | otherwise = error "newWires: the number of qubits must be nonnegative"
{-# INLINABLE newWires #-}

-- | Return new qubits which are initialized to |0\>.
ancillae :: MonadQuantumBase w m => Int -> m (Seq w)
ancillae n
  | n >= 0 = replicateParallelQ n ancilla
  | otherwise = error "ancillae: the number of ancillae must be nonnegative"
{-# INLINABLE ancillae #-}

-- | Apply the Pauli-Z gate.
applyZ :: MonadQuantumBase w m => Bit w -> m ()
applyZ (BitConst False) = return ()
applyZ (BitConst True) = negateGlobalPhase
applyZ (BitWire neg w) = rawApplyZ w neg

-- | Apply the S gate, or its inverse if the second argument is True.
applyS :: MonadQuantumBase w m => Bit w -> Bool -> m ()
applyS (BitConst False) _ = return ()
applyS (BitConst True) inv = rotateGlobalPhase90 inv
applyS (BitWire neg w) inv = rawApplyS w neg inv

-- | Apply the T gate, or its inverse if the second argument is True.
applyT :: MonadQuantumBase w m => Bit w -> Bool -> m ()
applyT (BitConst False) _ = return ()
applyT (BitConst True) inv = rotateGlobalPhase45 inv
applyT (BitWire neg w) inv = rawApplyT w neg inv

-- | Apply controlled Pauli-Z.
--
-- The two wires must be different.
applyZC :: MonadQuantumBase w m => Bit w -> Bit w -> m ()
applyZC (BitConst False) _ = return ()
applyZC (BitConst True) b = applyZ b
applyZC _ (BitConst False) = return ()
applyZC a (BitConst True) = applyZ a
applyZC (BitWire nega a) (BitWire negb b) = rawApplyZC a nega b negb

-- | @cnotWire w w1@ applies CNOT (controlled Pauli-X) with target @w@ controlled by @w1@.
--
-- @w@ and @w1@ must not refer to the same wire.
cnotWire :: MonadQuantumBase w m => w -> Bit w -> m ()
cnotWire _ (BitConst False) = return ()
cnotWire w (BitConst True) = applyX w
cnotWire w (BitWire neg w1) = rawCnotWire w w1 neg

-- | Given two sequences of wires, apply CNOT to each pair of wires at the corresponding position.
--
-- Precondition:
--
-- * The given wires are all distinct.
cnotWires :: MonadQuantumBase w m => Seq w -> Seq (Bit w) -> m ()
cnotWires targ ctrl =
    if Seq.length targ /= Seq.length ctrl then
      error "cnotWires: lengths do not match"
    else if null cnots then
      if null nots then
        return ()
      else
        mapParM_ applyX nots
    else
      if null nots then
        rawCnotWires cnots
      else
        parallel_
          (rawCnotWires cnots)
          (mapParM_ applyX nots)
  where
    pairs = zip (Foldable.toList targ) (Foldable.toList ctrl)
    cnots = [(t, c, neg) | (t, BitWire neg c) <- pairs]
    nots = [t | (t, BitConst True) <- pairs]

-- | @with_ prepare body@ is the sequential composition of @prepare@, @body@, and the inverse of @prepare@.
-- The result of @prepare@ is discarded.
--
-- Precondition:
--
-- * The composition restores all the ancillae introduced by @prepare@ to the initial state |0\>.
with_ :: MonadQuantumBase w m => m () -> m a -> m a
with_ prepare body =
    with prepare (const body)
{-# INLINABLE with_ #-}

-- | Apply given two circuits, possibly in parallel.
-- Implementations may choose to apply the given circuits sequentially.
--
-- Precondition:
--
-- * The two circuits do not refer to the same wire.
parallel :: MonadQuantumBase w m => m a -> m b -> m (a, b)
parallel ma mb =
    bindParallel ma (\a -> (\b -> (a, b)) <$> mb)

-- | The same as `parallel`, but discards the result.
parallel_ :: MonadQuantumBase w m => m a -> m b -> m ()
parallel_ a b = void (parallel a b)
{-# INLINABLE parallel_ #-}

-- Although any instance of MonadQuantumBase is an applicative functor,
-- @pure@ may not be an identity of @\mf mx -> uncurry id <$> parallel mf mx@
-- because @parallel@ may compress the control context.
-- This is why we define two cases here.

data Parallel w m a = ParallelPure a | Parallel (m a)
  deriving Functor

getParallel :: Applicative m => Parallel w m a -> m a
getParallel (ParallelPure x) = Applicative.pure x
getParallel (Parallel mx) = mx

instance MonadQuantumBase w m => Applicative (Parallel w m) where
  pure = ParallelPure
  ParallelPure f <*> mx = f <$> mx
  Parallel mf <*> ParallelPure x = Parallel (($ x) <$> mf)
  Parallel mf <*> Parallel mx = Parallel (uncurry id <$> parallel mf mx)

-- | Apply the circuits in the given container possibly in parallel, and return the results in the container of the same form.
--
-- Precondition:
--
-- No two of the given circuits refer to the same wire.
parallels :: (MonadQuantumBase w m, Traversable t) => t (m a) -> m (t a)
parallels ms = getParallel $ Traversable.traverse Parallel ms
{-# INLINABLE parallels #-}

-- | Apply the circuits in the given container possibly in parallel, and discard the results.
--
-- Precondition:
--
-- No two of the given circuits refer to the same wire.
parallels_ :: (MonadQuantumBase w m, Foldable t) => t (m a) -> m ()
parallels_ ms = getParallel $ Foldable.traverse_ Parallel ms
{-# INLINABLE parallels_ #-}

-- | Apply the circuit to the elements in the given container possibly in parallel,
-- and return the results in the container of the same form.
--
-- Precondition:
--
-- No two of the given circuits refer to the same wire.
mapParM :: (MonadQuantumBase w m, Traversable t) => (a -> m b) -> t a -> m (t b)
mapParM f xs = getParallel $ Traversable.traverse (Parallel . f) xs
{-# INLINABLE mapParM #-}

-- | Apply the circuit to the elements in the given container possibly in parallel, and discard the results.
--
-- Precondition:
--
-- No two of the given circuits refer to the same wire.
mapParM_ :: (MonadQuantumBase w m, Foldable t) => (a -> m b) -> t a -> m ()
mapParM_ f xs = getParallel $ Foldable.traverse_ (Parallel . f) xs
{-# INLINABLE mapParM_ #-}

-- | `mapParM` with its arguments flipped.
forParM :: (MonadQuantumBase w m, Traversable t) => t a -> (a -> m b) -> m (t b)
forParM = flip mapParM
{-# INLINABLE forParM #-}

-- | `mapParM_` with its arguments flipped.
forParM_ :: (MonadQuantumBase w m, Foldable t) => t a -> (a -> m b) -> m ()
forParM_ = flip mapParM_
{-# INLINABLE forParM_ #-}

-- | Apply the swap gate to two wires.
--
-- The two wires must be distinct.
swapWire :: MonadQuantumBase w m => w -> w -> m ()
swapWire w1 w2 =
    with_ (cnotWire w1 (bit w2)) $
      cnotWire w2 (bit w1)
{-# INLINABLE swapWire #-}

-- | Given two sequences of wires, apply the swap gate to each pair of wires at the corresponding position.
--
-- All the given wires must be distinct.
swapWires :: MonadQuantumBase w m => Seq w -> Seq w -> m ()
swapWires ws1 ws2 =
    if Seq.length ws1 /= Seq.length ws2 then
      error "swapWires: lengths do not match"
    else
      with_ (cnotWires ws1 (bit <$> ws2)) $
        cnotWires ws2 (bit <$> ws1)
{-# INLINABLE swapWires #-}

-- |Convert a `Bit` to a wire, possibly consuming the given Bit.
--
-- If the input `Bit` is a wire, the returned wire is the same as the input wire.
-- Therefore, the input bit should be considered as “consumed”
-- while the wire returned by this function is being used.

bitToWire :: MonadQuantumBase w m => Bit w -> m w
bitToWire (BitConst b) = withoutCtrls $ do
    w <- ancilla
    when b $ applyX w
    return w
bitToWire (BitWire neg w) = withoutCtrls $ do
    when neg $ applyX w
    return w
{-# INLINABLE bitToWire #-}

-- |@cnotBit t c@ returns a `Bit` representing @t XOR c@, possibly consuming @t@.
--
-- If @t@ is a wire, then this wire is returned.
-- Therefore, @t@ should be considered as “consumed”
-- while the bit returned by this function is being used.

cnotBit :: MonadQuantumBase w m => Bit w -> Bit w -> m (Bit w)
cnotBit (BitConst t) (BitConst c) =
    return $ BitConst (t /= c)
cnotBit t c = withoutCtrls $ do
    t' <- bitToWire t
    cnotWire t' c
    return $ bit t'
{-# INLINABLE cnotBit #-}

-- Instance for ReaderT
-- Requires UndecidableInstances.

instance MonadQuantumBase w m => MonadQuantumBase w (ReaderT r m) where

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

  rawCnotWire w w1 neg = lift $ rawCnotWire w w1 neg
  {-# INLINABLE rawCnotWire #-}

  rawCnotWires ws = lift $ rawCnotWires ws
  {-# INLINABLE rawCnotWires #-}

  with prepare body = ReaderT $ \r ->
      with (runReaderT prepare r) $ \a ->
        runReaderT (body a) r
  {-# INLINABLE with #-}

  bindParallel m1 m2 = ReaderT $ \r ->
      bindParallel (runReaderT m1 r) (\a -> runReaderT (m2 a) r)
  {-# INLINABLE bindParallel #-}

  invert m = ReaderT $ \r ->
      invert (runReaderT m r)
  {-# INLINABLE invert #-}

  withoutCtrls m = ReaderT $ \r ->
      withoutCtrls (runReaderT m r)

  replicateQ n m = ReaderT $ \r ->
      replicateQ n (runReaderT m r)
  {-# INLINABLE replicateQ #-}

  replicateParallelQ n m = ReaderT $ \r ->
      replicateParallelQ n (runReaderT m r)
  {-# INLINABLE replicateParallelQ #-}

  replicateQ_ n m = ReaderT $ \r ->
      replicateQ_ n (runReaderT m r)
  {-# INLINABLE replicateQ_ #-}

  replicateParallelQ_ n m = ReaderT $ \r ->
      replicateParallelQ_ n (runReaderT m r)
  {-# INLINABLE replicateParallelQ_ #-}

  genericReplicateQ_ n m = ReaderT $ \r ->
      genericReplicateQ_ n (runReaderT m r)
  {-# INLINABLE genericReplicateQ_ #-}

  genericReplicateParallelQ_ n m = ReaderT $ \r ->
      genericReplicateParallelQ_ n (runReaderT m r)
  {-# INLINABLE genericReplicateParallelQ_ #-}

-- Instance for MemoNullT
-- Requires UndecidableInstances.

instance MonadQuantumBase w m => MonadQuantumBase w (MemoNullT k m) where

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

  rawCnotWire w w1 neg = lift $ rawCnotWire w w1 neg
  {-# INLINABLE rawCnotWire #-}

  rawCnotWires ws = lift $ rawCnotWires ws
  {-# INLINABLE rawCnotWires #-}

  with prepare body = MemoNullT $
      with (runMemoNullT prepare) $ \a ->
        runMemoNullT (body a)
  {-# INLINABLE with #-}

  bindParallel m1 m2 = MemoNullT $
      bindParallel (runMemoNullT m1) (\a -> runMemoNullT (m2 a))
  {-# INLINABLE bindParallel #-}

  invert m = MemoNullT $
      invert (runMemoNullT m)
  {-# INLINABLE invert #-}

  withoutCtrls m = MemoNullT $
      withoutCtrls (runMemoNullT m)

  replicateQ n m = MemoNullT $
      replicateQ n (runMemoNullT m)
  {-# INLINABLE replicateQ #-}

  replicateParallelQ n m = MemoNullT $
      replicateParallelQ n (runMemoNullT m)
  {-# INLINABLE replicateParallelQ #-}

  replicateQ_ n m = MemoNullT $
      replicateQ_ n (runMemoNullT m)
  {-# INLINABLE replicateQ_ #-}

  replicateParallelQ_ n m = MemoNullT $
      replicateParallelQ_ n (runMemoNullT m)
  {-# INLINABLE replicateParallelQ_ #-}

  genericReplicateQ_ n m = MemoNullT $
      genericReplicateQ_ n (runMemoNullT m)
  {-# INLINABLE genericReplicateQ_ #-}

  genericReplicateParallelQ_ n m = MemoNullT $
      genericReplicateParallelQ_ n (runMemoNullT m)
  {-# INLINABLE genericReplicateParallelQ_ #-}
