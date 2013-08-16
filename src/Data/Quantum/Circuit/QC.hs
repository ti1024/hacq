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

module Data.Quantum.Circuit.QC (
    GateToQCBuilder(..),
    QCComponentId, QCComponent(..), Step(..), Item(..),
    QCBuilder, runQCBuilder, runQCBuilderToBlazeBuilder) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Control.Applicative (Applicative)
import Control.Monad.State.Strict (StateT, evalStateT)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Writer.Strict (Writer, runWriter)
import qualified Control.Monad.Writer.Strict as Writer
import Data.ByteString.Lazy (ByteString)
import qualified Data.Foldable as Foldable
import Data.Functor ((<$>))
import Data.Monoid (Monoid, (<>), mempty)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified System.IO as IO

import Data.Quantum.Circuit.Class (Wire(..), IsGate(..), Gate(..), IsCircuit(..))
import Util ((!))

type QCComponentId = Int

data QCComponent g = QCComponent !(Set Integer) !(Seq (Step g)) -- steps are run sequentially
newtype Step g = Step (Seq (Item g)) -- items are run in parallel inside each step
data Item g = IGate !(g Wire) | IComponent !QCComponentId !(Set Integer)

parallelSteps :: Step g -> Step g -> Step g
parallelSteps (Step items1) (Step items2) =
    Step (items1 >< items2)

newtype QCBuilder g a = QCBuilder {
  unwrapQCBuilder :: StateT QCComponentId (Writer (Seq (QCComponentId, QCComponent g))) a
} deriving (Functor, Applicative, Monad)

instance IsGate g => IsCircuit g Wire (QCBuilder g (QCComponent g)) where

  empty =
      return $ QCComponent Set.empty Seq.empty
  {-# INLINABLE empty #-}

  singleton g =
      return $ QCComponent touched $ Seq.singleton $ Step $ Seq.singleton $ IGate g
    where
      touched = Set.fromList (getWire <$> Foldable.toList g)
  {-# INLINABLE singleton #-}

  newWire (Wire wi) _ = return $ QCComponent (Set.singleton wi) Seq.empty
  {-# INLINABLE newWire #-}
  ancilla (Wire wi) _ = return $ QCComponent (Set.singleton wi) Seq.empty
  {-# INLINABLE ancilla #-}

  sequential (QCBuilder m1) (QCBuilder m2) = QCBuilder $ do
      QCComponent touched1 l1 <- m1
      QCComponent touched2 l2 <- m2
      return $ QCComponent (Set.union touched1 touched2) (l1 >< l2)
  {-# INLINABLE sequential #-}

  parallel (QCBuilder m1) (QCBuilder m2) = QCBuilder $ do
      comp1@(QCComponent touched1 steps1) <- m1
      comp2@(QCComponent touched2 steps2) <- m2
      if not $ Set.null $ Set.intersection touched1 touched2 then
        error "Circuit.QC.parallel: subcircuits touch the same wire"
      else if Seq.null steps1 then
        return comp2
      else if Seq.null steps2 then
        return comp1
      else do
        step1 <-
          if Seq.length steps1 == 1 then
            return $ steps1 ! 0
          else do
            compId1 <- State.get
            State.put $! compId1 + 1
            Writer.tell $! Seq.singleton (compId1, comp1)
            return $ Step $ Seq.singleton $ IComponent compId1 touched1
        step2 <-
          if Seq.length steps2 == 1 then
            return $ steps2 ! 0
          else do
            compId2 <- State.get
            State.put $! compId2 + 1
            Writer.tell $! Seq.singleton (compId2, comp2)
            return $ Step $ Seq.singleton $ IComponent compId2 touched2
        return $ QCComponent (Set.union touched1 touched2) $ Seq.singleton $ parallelSteps step1 step2
  {-# INLINABLE parallel #-}

class GateToQCBuilder g where
  gateToQCBuilder :: g Wire -> Builder

instance GateToQCBuilder Gate where
  gateToQCBuilder (GateX (Wire wi)) =
      fromString "tof " <> fromShow wi
  gateToQCBuilder (GateXC (Wire wi) (Wire wi1) neg1) =
      fromString "tof " <> fromShow wi1 <> (if neg1 then fromChar '\'' else mempty) <> fromChar ' ' <> fromShow wi
  gateToQCBuilder (GateXCC (Wire wi) (Wire wi1) neg1 (Wire wi2) neg2) =
      fromString "tof " <>
      fromShow wi1 <> (if neg1 then fromChar '\'' else mempty) <> fromChar ' ' <>
      fromShow wi2 <> (if neg2 then fromChar '\'' else mempty) <> fromChar ' ' <> fromShow wi
  gateToQCBuilder (GateH (Wire wi)) =
      fromString "H " <> fromShow wi
  gateToQCBuilder (GateY (Wire wi)) =
      fromString "Y " <> fromShow wi
  gateToQCBuilder (GateZ (Wire wi)) =
      fromString "Z " <> fromShow wi
  gateToQCBuilder (GateS (Wire wi) False) =
      fromString "P " <> fromShow wi
  gateToQCBuilder (GateS (Wire wi) True) =
      fromString "P* " <> fromShow wi
  gateToQCBuilder (GateT (Wire wi) False) =
      fromString "T " <> fromShow wi
  gateToQCBuilder (GateT (Wire wi) True) =
      fromString "T* " <> fromShow wi

runQCBuilder :: (IsGate g, GateToQCBuilder g) => IO.Newline -> QCBuilder g (QCComponent g) -> ByteString
runQCBuilder nl = toLazyByteString . runQCBuilderToBlazeBuilder newlineBuilder
  where
    newlineBuilder =
        case nl of
          IO.LF -> fromChar '\n'
          IO.CRLF -> fromString "\r\n"
{-# INLINABLE runQCBuilder #-}

runQCBuilderToBlazeBuilder :: GateToQCBuilder g => Builder -> QCBuilder g (QCComponent g) -> Builder
runQCBuilderToBlazeBuilder nl circuit =
    fromString ".v " <> vars <> nl <>
    fromString ".i " <> vars <> nl <>
    fromString ".o " <> vars <> nl <>
    fromString ".ol " <> vars <> nl <>
    Foldable.foldMap (uncurry (subcomponentToQCBuilder nl)) subcomps <>
    fromString "BEGIN" <> nl <>
    stepsToQCBuilder nl mainSteps <>
    fromString "END" <> nl
  where
    (QCComponent mainTouched mainSteps, subcomps) =
        runWriter $ evalStateT (unwrapQCBuilder circuit) 0
    vars =
        touchedToQCBuilder mainTouched
{-# INLINABLE runQCBuilderToBlazeBuilder #-}

subcomponentToQCBuilder :: GateToQCBuilder g => Builder -> QCComponentId -> QCComponent g -> Builder
subcomponentToQCBuilder nl compId (QCComponent touched steps) =
    fromString "BEGIN C" <> fromShow compId <> fromChar '(' <> touchedToQCBuilder touched <> fromChar ')' <> nl <>
    stepsToQCBuilder nl steps <>
    fromString "END C" <> fromShow compId <> nl

stepsToQCBuilder :: GateToQCBuilder g => Builder -> Seq (Step g) -> Builder
stepsToQCBuilder nl =
    Foldable.foldMap (\step -> stepToQCBuilder step <> nl)
  where
    stepToQCBuilder (Step items) =
        builderFromList (fromChar ';') itemToQCBuilder (Foldable.toList items)
    itemToQCBuilder (IGate g) = gateToQCBuilder g
    itemToQCBuilder (IComponent compId touched) =
        fromChar 'C' <> fromShow compId <> fromChar ' ' <> touchedToQCBuilder touched

touchedToQCBuilder :: Set Integer -> Builder
touchedToQCBuilder =
    builderFromList (fromChar ' ') fromShow . Set.toAscList

builderFromList :: Builder -> (a -> Builder) -> [a] -> Builder
builderFromList _ _ [] = mempty
builderFromList separator build (x : xs) = go x xs
  where
    go y [] = build y
    go y (y' : ys) = build y <> separator <> go y' ys
