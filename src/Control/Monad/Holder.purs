module Control.Monad.Holder (class PartialMonoid, class MonadHolder, HolderT, partialEmpty, partialAppend, reserve, runHolderT) where

import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, get, put, state)
import Control.Monad.State.Trans (StateT, evalStateT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Translatable (class MonadTranslatable, translate)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, bind, discard, pure, (<$>), (<<<))

class PartialMonoid a where
  partialEmpty :: a
  partialAppend :: a -> a -> Maybe a

class (Monad m, PartialMonoid r) <= MonadHolder r m | m -> r where
  reserve :: forall a. r -> m a -> m (Maybe a)

newtype HolderT r m a = HolderT (StateT r m a)

derive instance newtypeHolderT :: Newtype (HolderT r m a) _

derive newtype instance functorHolderT ::
  Functor m => Functor (HolderT r m)
derive newtype instance applyHolderT :: Monad m => Apply (HolderT r m)
derive newtype instance applicativeHolderT ::
  Monad m => Applicative (HolderT r m)
derive newtype instance bindHolderT :: Monad m => Bind (HolderT r m)
derive newtype instance monadHolderT :: Monad m => Monad (HolderT r m)

instance monadHolderHolderT ::
    (Monad m, PartialMonoid r) => MonadHolder r (HolderT r m) where
  reserve resources action = HolderT do
    prevStatus <- get
    case partialAppend prevStatus resources of
      Just newResources -> do
        put newResources
        Just <$> unwrap action
      Nothing -> pure Nothing

runHolderT ::
  forall r m a. PartialMonoid r => Monad m => HolderT r m a -> m a
runHolderT (HolderT h) = evalStateT h partialEmpty

derive newtype instance monadRecHolderT ::
  MonadRec m => MonadRec (HolderT r m)
derive newtype instance monadThrowHolderT ::
  MonadThrow e m => MonadThrow e (HolderT r m)
derive newtype instance monadErrorHolderT ::
  MonadError e m => MonadError e (HolderT r m)
derive newtype instance monadEffHolderT ::
  MonadEff e m => MonadEff e (HolderT r m)

instance monadTransHolderT ::
    PartialMonoid r => MonadTrans (HolderT r) where
  lift = HolderT <<< lift

instance monadStateHolderT ::
    (PartialMonoid r, MonadState r' m) => MonadState r' (HolderT r m) where
  state = lift <<< state

instance monadTranslatableHolderT ::
    (PartialMonoid r, Monad m, MonadTranslatable m n) =>
      MonadTranslatable (HolderT r m) n where
  translate = translate <<< runHolderT
