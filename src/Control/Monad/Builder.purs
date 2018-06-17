module Control.Monad.Builder (class MonadCreate, class MonadBuilder, create, localCreate) where

import Control.Monad.Runnable (class MonadRunnable, run)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (class MonadWriter, WriterT, execWriterT)
import Data.Monoid (class Monoid)
import Prelude (class Monad, (<<<))

class Monad m <= MonadCreate o m | m -> o where
  create :: forall a. m a -> o
  localCreate :: forall a. m a -> m o

instance monadFactoryWriterT ::
    (Monoid o, MonadRunnable m) => MonadCreate o (WriterT o m) where
  create = run <<< execWriterT
  localCreate = lift <<< execWriterT

class (MonadCreate o m, MonadWriter o m) <= MonadBuilder o m | m -> o

instance monadBuilderWriterT ::
    (Monoid o, MonadRunnable m) => MonadBuilder o (WriterT o m)
