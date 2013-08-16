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

module Control.Monad.Memo.Class (MonadMemo(..)) where

import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)
import Data.Typeable (Typeable)

class Monad m => MonadMemo k m | m -> k where
  memoize :: Typeable a => k -> m a -> m a

-- Instance for ReaderT
-- This instance requires UndecidableInstances.

instance (MonadMemo (k, r) m) => MonadMemo k (ReaderT r m) where
  memoize k m = ReaderT $ \r -> memoize (k, r) $ runReaderT m r
  {-# INLINABLE memoize #-}
