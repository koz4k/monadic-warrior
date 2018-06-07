module Creep.State (CreepState, ThreadId, addThread, getThreadCount, hasThread, initState, initThreadId, nextThreadId, removeThread, runThread) where

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Except (class MonadError, throwError)
import Control.Monad.State (class MonadState, get, put)
import Creep.Exec (ExecError(..))
import Data.Argonaut.Core (JObject, foldJsonObject, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Array (deleteAt, findIndex, length, snoc, updateAt, (!!))
import Data.Either (Either)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..), fst)
import Prelude (class Eq, class Ord, Unit, bind, discard, pure, unit, when, ($), (+), (-), (<$>), (<<<), (<=<), (==))

newtype ThreadId = ThreadId Int

derive instance newtypeThreadId :: Newtype ThreadId _
derive newtype instance eqThreadId :: Eq ThreadId
derive newtype instance ordThreadId :: Ord ThreadId
derive newtype instance encodeJsonThreadId :: EncodeJson ThreadId
derive newtype instance decodeJsonThreadId :: DecodeJson ThreadId

type Threads t = Array (Tuple ThreadId t)

newtype CreepState t =
  CreepState {threads :: Threads t}

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

initThreadId :: ThreadId
initThreadId = ThreadId 0

nextThreadId :: ThreadId -> ThreadId
nextThreadId (ThreadId i) = ThreadId $ i + 1

initState :: forall t. t -> CreepState t
initState thread =
  CreepState {threads: [Tuple initThreadId thread]}

runThread ::
  forall t m e.
    MonadState (CreepState t) m => MonadError ExecError m =>
    MonadEff (random :: RANDOM | e) m =>
      (t -> m t) -> m Unit
runThread execute = do
  {threads} <- unwrap <$> get
  index <- liftEff $ randomInt 0 $ length threads - 1
  case threads !! index of
    Nothing -> throwError $ ErrorMessage "empty thread queue"
    Just (Tuple id thread) -> do
      thread' <- execute thread
      -- Threads could have changed, refresh.
      {threads: threads'} <- unwrap <$> get
      case findThreadIndex id threads' of
        Nothing ->
          -- Thread killed itself, return.
          pure unit
        Just index' -> case updateAt index' (Tuple id thread') threads' of
          Nothing -> throwError $ ErrorMessage "thread index out of bounds"
          Just threads'' -> do
            put $ creepState threads''

getThreadCount :: forall t m. MonadState (CreepState t) m => m Int
getThreadCount = length <<< (_.threads) <<< unwrap <$> get

addThread ::
  forall t m.
    MonadState (CreepState t) m => MonadError ExecError m =>
      ThreadId -> t -> m Unit
addThread threadId thread = do
  {threads} <- unwrap <$> get
  when (isJust $ findThreadIndex threadId threads) $
    throwError $ ErrorMessage "thread already exists"
  put $ CreepState {threads: snoc threads $ Tuple threadId thread}

removeThread ::
  forall t m.
    MonadState (CreepState t) m => MonadError ExecError m =>
      ThreadId -> m Unit
removeThread threadId = do
  {threads} <- unwrap <$> get
  case findThreadIndex threadId threads of
    Nothing -> throwError $ ErrorMessage "thread not found"
    Just threadIndex -> do
      case deleteAt threadIndex threads of
        Nothing -> throwError $ ErrorMessage "thread index out of bounds"
        Just threads' ->
          put $ CreepState {threads: threads'}

hasThread :: forall t m. MonadState (CreepState t) m => ThreadId -> m Boolean
hasThread threadId =
  isJust <<< findThreadIndex threadId <<< (_.threads) <<< unwrap <$> get

findThreadIndex :: forall t. ThreadId -> Threads t -> Maybe Int
findThreadIndex threadId = findIndex $ (_ == threadId) <<< fst

creepState :: forall t. Threads t -> CreepState t
creepState threads = CreepState {threads}
