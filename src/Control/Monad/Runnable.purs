module Control.Monad.Runnable (class MonadRunnable, run) where

import Control.Monad.Translatable (class MonadTranslatable, translate)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Prelude (class Monad, (<<<))

class Monad m <= MonadRunnable m where
  run :: forall a. m a -> a

instance monadRunnableMonadTranslatableToIdentity ::
    (Monad m, MonadTranslatable m Identity) => MonadRunnable m where
  run = unwrap <<< (translate :: forall a. m a -> Identity a)
