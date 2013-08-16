{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Memo (module Control.Monad.Memo.Class,
    Memo, runMemo, runWithoutMemo) where

import Control.Applicative (Applicative)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.ST
import Data.Dynamic (Dynamic, toDyn, fromDyn)
import Data.Hashable (Hashable)
import Data.HashTable.ST.Basic (HashTable)
import qualified Data.HashTable.ST.Basic as HashTable

import Control.Monad.Ask.Class (MonadAsk)
import qualified Control.Monad.Ask.Class as Ask
import Control.Monad.Memo.Class
import Data.Void

newtype Memo s r k a = Memo {
  unwrapMemo :: ReaderT (HashTable s k Dynamic, r) (ST s) a
} deriving (Functor, Applicative, Monad)

instance (Eq k, Hashable k) => MonadMemo k (Memo s r k) where
  memoize k m = Memo $ ReaderT $ \r'@(memoTable, _) -> do
      res <- HashTable.lookup memoTable k
      case res of
        Just dyna ->
          return $ fromDyn dyna (error "memoize: Type mismatch")
        Nothing -> do
          a <- runReaderT (unwrapMemo m) r'
          HashTable.insert memoTable k (toDyn a)
          return a

runMemo :: (forall s. Memo s r k a) -> r -> a
runMemo m r = runST $ do
    memoTable <- HashTable.new
    a <- runReaderT (unwrapMemo m) (memoTable, r)
    return a

runWithoutMemo :: (forall s. Memo s r Void a) -> r -> a
runWithoutMemo = runMemo

instance MonadAsk r (Memo s r k) where
  ask = Memo $ Reader.asks snd
