module Control.Monad.Translatable (class MonadTranslatable, translate) where

import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.State (StateT, evalStateT)
import Data.Bifunctor (lmap)
import Data.Default (class Default, def)
import Data.Identity (Identity)
import Effect (Effect)
import Error (class ErrorDetails, renderError)
import Prelude (class Monad, flip, identity, map, ($), (<<<))

class (Monad m, Monad n) <= MonadTranslatable m n where
  translate :: forall a. m a -> n a

instance monadTranslatableIdentity :: MonadTranslatable Identity Identity where
  translate = identity

instance monadTranslatableEffect :: MonadTranslatable Effect Effect where
  translate = identity

instance monadTranslatableStateT ::
    (Default s, Monad m, MonadTranslatable m n) =>
      MonadTranslatable (StateT s m) n where
  translate = translate <<< flip evalStateT def

instance monadTranslatableExceptT ::
    (ErrorDetails e, Monad m, MonadTranslatable m n) =>
      MonadTranslatable (ExceptT e m) (ExceptT String n) where
  translate = mapExceptT $ translate <<< map (lmap renderError)
