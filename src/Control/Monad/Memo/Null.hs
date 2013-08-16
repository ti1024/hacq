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

module Control.Monad.Memo.Null (module Control.Monad.Memo.Class,
    MemoNullT(..)) where

import Control.Applicative (Applicative)
import Control.Monad.Trans (MonadTrans(lift))

import Control.Monad.Ask.Class (MonadAsk)
import Control.Monad.Memo.Class

newtype MemoNullT k m a = MemoNullT {
  runMemoNullT :: m a
} deriving (Functor, Applicative, Monad, MonadAsk r)

instance MonadTrans (MemoNullT k) where
  lift = MemoNullT

instance Monad m => MonadMemo k (MemoNullT k m) where
  memoize _ m = m
