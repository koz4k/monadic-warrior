module Control.Monad.Translatable (class MonadTranslatable, translate) where

import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Bifunctor (lmap)
import Data.Default (class Default, def)
import Error (class ErrorDetails, renderError)
import Prelude (class Monad, flip, id, map, ($), (<<<))

class (Monad m, Monad n) <= MonadTranslatable m n where
  translate :: forall a. m a -> n a

instance monadTranslatableReflexivity :: Monad m => MonadTranslatable m m where
  translate = id

instance monadTranslatableMonadTrans ::
    (Monad m, MonadTrans t, Monad (t m)) => MonadTranslatable m (t m) where
  translate = lift

instance monadTranslatableStateT ::
    (Default s, Monad m, MonadTranslatable m n) =>
      MonadTranslatable (StateT s m) n where
  translate = translate <<< flip evalStateT def

instance monadTranslatableExceptT ::
    (ErrorDetails e, Monad m, MonadTranslatable m n) =>
      MonadTranslatable (ExceptT e m) (ExceptT String n) where
  translate = mapExceptT $ translate <<< map (lmap renderError)
