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

module Control.Monad.Ask.Class (MonadAsk(..), asks) where

import Control.Applicative (Applicative)
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.Trans (lift)
import qualified Control.Monad.Writer.Lazy as LWriter
import qualified Control.Monad.Writer.Strict as SWriter
import Data.Functor ((<$>))
import Data.Monoid (Monoid)

class (Applicative m, Monad m) => MonadAsk r m | m -> r where
  ask :: m r

asks :: MonadAsk r m => (r -> a) -> m a
asks f = f <$> ask

instance (Applicative m, Monad m) => MonadAsk r (ReaderT r m) where
  ask = Reader.ask

instance (Monoid w, MonadAsk r m) => MonadAsk r (LWriter.WriterT w m) where
  ask = lift ask

instance (Monoid w, MonadAsk r m) => MonadAsk r (SWriter.WriterT w m) where
  ask = lift ask
