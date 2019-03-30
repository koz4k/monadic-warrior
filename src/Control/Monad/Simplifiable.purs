module Control.Monad.Simplifiable (class MonadSimplifiable, simplify) where

import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.State (StateT, evalStateT)
import Data.Bifunctor (lmap)
import Data.Default (class Default, def)
import Data.Identity (Identity)
import Effect (Effect)
import Error (class ErrorDetails, renderError)
import Prelude (class Monad, flip, identity, map, ($), (<<<))

class (Monad m, Monad n) <= MonadSimplifiable m n where
  simplify :: forall a. m a -> n a

instance monadSimplifiableIdentity :: MonadSimplifiable Identity Identity where
  simplify = identity

instance monadSimplifiableEffect :: MonadSimplifiable Effect Effect where
  simplify = identity

instance monadSimplifiableStateT ::
    (Default s, Monad m, MonadSimplifiable m n) =>
      MonadSimplifiable (StateT s m) n where
  simplify = simplify <<< flip evalStateT def

instance monadSimplifiableExceptT ::
    (ErrorDetails e, Monad m, MonadSimplifiable m n) =>
      MonadSimplifiable (ExceptT e m) (ExceptT String n) where
  simplify = mapExceptT $ simplify <<< map (lmap renderError)
