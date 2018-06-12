module Creep.State (CreepState, addThread, getThreadCount, hasThread, initState, removeThread, runThreads) where

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Except (class MonadError, throwError)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (class MonadState, get, put)
import Creep.Exec (ExecError(..))
import Data.Argonaut.Core (JObject, foldJsonObject, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Array (delete, deleteAt, findIndex, intersect, length, snoc, unsafeIndex, unzip, updateAt, (!!))
import Data.Either (Either)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, flip, map, pure, unit, when, ($), (-), (<$>), (<<<), (<=<), (==), (>>=))

type Threads t = Array (Tuple Int t)

newtype CreepState t = CreepState {threads :: Threads t}

derive instance newtypeCreepState :: Newtype (CreepState t) _

instance encodeJsonCreepState
  :: EncodeJson t => EncodeJson (CreepState t) where
  encodeJson (CreepState {threads}) = 
    "threads" := encodeJson threads ~>
    jsonEmptyObject

instance decodeJsonCreepState
  :: DecodeJson t => DecodeJson (CreepState t) where
  decodeJson = foldJsonObject (throwError "expected an object") \object ->
    creepState <$> decodeField object "threads"
    where
      decodeField ::
        forall a. DecodeJson a => JObject -> String -> Either String a
      decodeField object = decodeJson <=< (object .? _)

initState :: forall t. t -> CreepState t
initState thread =
  CreepState {threads: [Tuple 0 thread]}

runThreads ::
  forall t m e.
    MonadRec m => MonadState (CreepState t) m => MonadError ExecError m =>
    MonadEff (random :: RANDOM | e) m =>
      (t -> m t) -> m Unit
runThreads execute = do
  threadIds <- getThreadIds
  flip tailRecM threadIds $ \threadIds' ->
    let threadCount = length threadIds'
    in if threadCount == 0
      then pure $ Done unit
      else do
        threadId <- map (unsafePartial unsafeIndex threadIds') $
          liftEff $ randomInt 0 $ threadCount - 1
        runThread threadId
        threadIds'' <- getThreadIds
        pure $ Loop $ delete threadId threadIds' `intersect` threadIds''
  where
    runThread threadId = do
      getThread threadId >>= case _ of
        Nothing -> throwError $ ErrorMessage "thread not found"
        Just thread -> do
          thread' <- execute thread
          {threads} <- unwrap <$> get
          case findThreadIndex threadId threads of
            Nothing ->
              -- Thread killed itself, return.
              pure unit
            Just threadIndex ->
              case updateAt threadIndex (Tuple threadId thread') threads of
                Nothing ->
                  throwError $ ErrorMessage "thread index out of bounds"
                Just threads' -> put $ creepState threads'

getThreadCount :: forall t m. MonadState (CreepState t) m => m Int
getThreadCount = length <$> getThreadIds

getThreadIds :: forall t m. MonadState (CreepState t) m => m (Array Int)
getThreadIds = fst <<< unzip <<< (_.threads) <<< unwrap <$> get

addThread ::
  forall t m.
    MonadState (CreepState t) m => MonadError ExecError m =>
      Int -> t -> m Unit
addThread threadId thread = do
  {threads} <- unwrap <$> get
  when (isJust $ findThreadIndex threadId threads) $
    throwError $ ErrorMessage "thread already exists"
  put $ CreepState {threads: snoc threads $ Tuple threadId thread}

removeThread ::
  forall t m.
    MonadState (CreepState t) m => MonadError ExecError m =>
      Int -> m Unit
removeThread threadId = do
  {threads} <- unwrap <$> get
  case findThreadIndex threadId threads of
    Nothing -> throwError $ ErrorMessage "thread not found"
    Just threadIndex -> do
      case deleteAt threadIndex threads of
        Nothing -> throwError $ ErrorMessage "thread index out of bounds"
        Just threads' ->
          put $ CreepState {threads: threads'}

hasThread :: forall t m. MonadState (CreepState t) m => Int -> m Boolean
hasThread = map isJust <<< getThread

getThread :: forall t m. MonadState (CreepState t) m => Int -> m (Maybe t)
getThread threadId =
  findThread threadId <<< (_.threads) <<< unwrap <$> get

findThread :: forall t. Int -> Threads t -> Maybe t
findThread threadId threads = do
  threadIndex <- findThreadIndex threadId threads
  snd <$> threads !! threadIndex

findThreadIndex :: forall t. Int -> Threads t -> Maybe Int
findThreadIndex threadId = findIndex $ (_ == threadId) <<< fst

creepState :: forall t. Threads t -> CreepState t
creepState threads = CreepState {threads}
