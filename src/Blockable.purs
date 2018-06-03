module Blockable (class PartialMonoid, BlockableT, partialEmpty, partialAppend, block, reserve, runBlockableT, unblock) where

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, get, put, state)
import Control.Monad.State.Trans (StateT, evalStateT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, bind, discard, ($), (<<<), (=<<))

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
derive newtype instance monadThrowBlockableT ::
  MonadThrow e m => MonadThrow e (BlockableT s m)
derive newtype instance monadErrorBlockableT ::
  MonadError e m => MonadError e (BlockableT s m)

instance monadTransBlockableT ::
  PartialMonoid s => MonadTrans (BlockableT s) where
  lift = reserve partialEmpty

instance monadStateBlockableT ::
  (PartialMonoid s, MonadState s' m) => MonadState s' (BlockableT s m) where
  state = lift <<< state

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
    PartialMonoid s => Monad m => s -> m a -> BlockableT s m a
reserve status action = BlockableT do
  prevStatus <- get
  case partialAppend prevStatus status of
    Just newStatus -> put newStatus
    Nothing -> empty
  lift $ lift action
