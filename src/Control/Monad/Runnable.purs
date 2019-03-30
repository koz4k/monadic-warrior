module Control.Monad.Runnable (class MonadRunnable, run) where

import Control.Monad.Simplifiable (class MonadSimplifiable, simplify)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Prelude (class Monad, (<<<))

class Monad m <= MonadRunnable m where
  run :: forall a. m a -> a

instance monadRunnableMonadSimplifiableToIdentity ::
    (Monad m, MonadSimplifiable m Identity) => MonadRunnable m where
  run = unwrap <<< (simplify :: forall a. m a -> Identity a)
