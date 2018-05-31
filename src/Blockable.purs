module Blockable where

import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (get, put)
import Control.Monad.State.Trans (StateT, evalStateT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, bind, const, discard, pure, when, ($), (<<<), (=<<))

class PartialMonoid a where
  partialEmpty :: a
  partialAppend :: a -> a -> Maybe a

newtype BlockableT s m a = BlockableT (StateT s (MaybeT m) a)

derive instance newtypeBlockableT :: Newtype (BlockableT s m a) _

derive newtype instance functorBlockableT ::
  Functor m => Functor (BlockableT s m)
derive newtype instance applyBlockableT :: Monad m => Apply (BlockableT s m)
derive newtype instance applicativeBlockableT ::
  Monad m => Applicative (BlockableT s m)
derive newtype instance bindBlockableT :: Monad m => Bind (BlockableT s m)
derive newtype instance monadBlockableT :: Monad m => Monad (BlockableT s m)
derive newtype instance monadRecBlockableT ::
  MonadRec m => MonadRec (BlockableT s m)

instance monadTransBlockableT ::
  PartialMonoid s => MonadTrans (BlockableT s) where
  lift = reserve partialEmpty $ const false

runBlockableT ::
  forall s m a. PartialMonoid s => Monad m => BlockableT s m a -> m (Maybe a)
runBlockableT (BlockableT b) = runMaybeT $ evalStateT b partialEmpty

block :: forall s m a. Monad m => BlockableT s m a
block = BlockableT empty

unblock ::
  forall s m a.
    PartialMonoid s => Monad m => BlockableT s m a -> BlockableT s m (Maybe a)
unblock (BlockableT b) =
  BlockableT $ lift <<< lift <<< runMaybeT <<< evalStateT b =<< get

reserve ::
  forall s m a.
    PartialMonoid s => Monad m => s -> (a -> Boolean) -> m a -> BlockableT s m a
reserve status shouldRollback action = BlockableT do
  prevStatus <- get
  case partialAppend prevStatus status of
    Just newStatus -> put newStatus
    Nothing -> empty
  result <- lift $ lift action
  when (shouldRollback result) $ put prevStatus
  pure result
