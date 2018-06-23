module Threads (Threads, addThread, getThreadCount, hasThread, initThreads, removeThread, runThreads) where

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (class MonadState, get, put)
import Data.Array (delete, deleteAt, findIndex, intersect, length, snoc, unsafeIndex, unzip, updateAt, (!!))
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..), fst, snd)
import Error (class ErrorMessage, throwErrorMessage)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, flip, map, pure, unit, when, ($), (-), (<$>), (<<<), (==), (>>=))

type Threads t = Array (Tuple Int t)

initThreads :: forall t. t -> Threads t
initThreads thread = [Tuple 0 thread]

runThreads ::
  forall e m t.
    MonadRec m => MonadState (Threads t) m => ErrorMessage e =>
    MonadThrow e m => MonadEffect m =>
      (t -> m t) -> m Unit
runThreads execute = do
  threadIds <- getThreadIds
  flip tailRecM threadIds $ \threadIds' ->
    let threadCount = length threadIds'
    in if threadCount == 0
      then pure $ Done unit
      else do
        threadId <- map (unsafePartial unsafeIndex threadIds') $
          liftEffect $ randomInt 0 $ threadCount - 1
        runThread threadId
        threadIds'' <- getThreadIds
        pure $ Loop $ delete threadId threadIds' `intersect` threadIds''
  where
    runThread threadId = do
      getThread threadId >>= case _ of
        Nothing -> throwErrorMessage "thread not found"
        Just thread -> do
          thread' <- execute thread
          threads <- get
          case findThreadIndex threadId threads of
            Nothing ->
              -- Thread killed itself, return.
              pure unit
            Just threadIndex ->
              case updateAt threadIndex (Tuple threadId thread') threads of
                Nothing ->
                  throwErrorMessage "thread index out of bounds"
                Just threads' -> put threads'

getThreadCount :: forall m t. MonadState (Threads t) m => m Int
getThreadCount = length <$> getThreadIds

getThreadIds :: forall m t. MonadState (Threads t) m => m (Array Int)
getThreadIds = fst <<< unzip <$> get

addThread ::
  forall e m t.
    MonadState (Threads t) m => ErrorMessage e => MonadThrow e m =>
      Int -> t -> m Unit
addThread threadId thread = do
  threads <- get
  when (isJust $ findThreadIndex threadId threads) $
    throwErrorMessage "thread already exists"
  put $ snoc threads $ Tuple threadId thread

removeThread ::
  forall e m t.
    MonadState (Threads t) m => ErrorMessage e => MonadThrow e m =>
      Int -> m Unit
removeThread threadId = do
  threads <- get
  case findThreadIndex threadId threads of
    Nothing -> throwErrorMessage "thread not found"
    Just threadIndex -> do
      case deleteAt threadIndex threads of
        Nothing -> throwErrorMessage "thread index out of bounds"
        Just threads' ->
          put threads'

hasThread :: forall m t. MonadState (Threads t) m => Int -> m Boolean
hasThread = map isJust <<< getThread

getThread :: forall m t. MonadState (Threads t) m => Int -> m (Maybe t)
getThread threadId = findThread threadId <$> get

findThread :: forall t. Int -> Threads t -> Maybe t
findThread threadId threads = do
  threadIndex <- findThreadIndex threadId threads
  snd <$> threads !! threadIndex

findThreadIndex :: forall t. Int -> Threads t -> Maybe Int
findThreadIndex threadId = findIndex $ (_ == threadId) <<< fst
