module Creep.State (CreepState, initState, runThread) where

import Control.Monad.Except (class MonadError, throwError)
import Control.Monad.State (class MonadState, get, put)
import Creep.Exec (ExecError(..))
import Data.Array (head, singleton, snoc, tail)
import Data.Maybe (Maybe(..))
import Prelude (Unit, bind, ($), (>>=), (>>>))

type CreepState = Array

initState :: forall t. t -> CreepState t
initState = singleton

runThread ::
  forall t m.
    MonadState (CreepState t) m => MonadError ExecError m =>
      (t -> m t) -> m Unit
runThread execute = do
  get >>= head >>> case _ of
    Nothing -> throwError $ ErrorMessage "empty thread queue"
    Just plan -> do
      plan' <- execute plan
      get >>= tail >>> case _ of 
        Nothing ->
          throwError $ ErrorMessage "empty thread queue after executing thread"
        Just threadQueueTail ->
          put $ snoc threadQueueTail plan'
