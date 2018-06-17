module Control.Monad.Runnable (class MonadRunnable, run) where

import Control.Monad.State (StateT, evalStateT)
import Data.Default (class Default, def)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Prelude (class Monad, flip, (<<<))

class Monad m <= MonadRunnable m where
  run :: forall a. m a -> a

instance monadRunnableIdentity :: MonadRunnable Identity where
  run = unwrap

instance monadRunnableState ::
    (Default s, MonadRunnable m) => MonadRunnable (StateT s m) where
  run = run <<< flip evalStateT def
