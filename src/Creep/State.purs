module Creep.State (CreepState, getThreadCount, initState, runThread) where

import Control.Monad.Except (class MonadError, throwError)
import Control.Monad.State (class MonadState, get, put)
import Creep.Exec (ExecError(..))
import Data.Argonaut.Core (JObject, foldJsonObject, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Array (length, updateAt, (!!))
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Prelude (Unit, bind, flip, mod, ($), (+), (<$>), (<*>), (<<<), (<=<))

newtype ThreadId = ThreadId Int

derive instance newtypeThreadId :: Newtype ThreadId _
derive newtype instance encodeJsonThreadId :: EncodeJson ThreadId
derive newtype instance decodeJsonThreadId :: DecodeJson ThreadId

newtype CreepState t =
  CreepState { threads :: Array (Tuple ThreadId t)
             , currentIndex :: Int
             , newId :: ThreadId
             }

derive instance newtypeCreepState :: Newtype (CreepState t) _

instance encodeJsonCreepState
  :: EncodeJson t => EncodeJson (CreepState t) where
  encodeJson (CreepState {threads, currentIndex, newId}) = 
    "threads" := encodeJson threads ~>
    "currentIndex" := encodeJson currentIndex ~>
    "newId" := encodeJson newId ~>
    jsonEmptyObject

instance decodeJsonCreepState
  :: DecodeJson t => DecodeJson (CreepState t) where
  decodeJson = foldJsonObject (throwError "expected an object") \object ->
    creepState <$> decodeField object "threads"
               <*> decodeField object "currentIndex"
               <*> decodeField object "newId"
    where
      decodeField ::
        forall a. DecodeJson a => JObject -> String -> Either String a
      decodeField object = decodeJson <=< (object .? _)

initState :: forall t. t -> CreepState t
initState thread =
  CreepState { threads: [Tuple (ThreadId 0) thread]
             , currentIndex: 0
             , newId: ThreadId 1
             }

runThread ::
  forall t m.
    MonadState (CreepState t) m => MonadError ExecError m =>
      (t -> m t) -> m Unit
runThread execute = do
  {threads, currentIndex} <- unwrap <$> get
  case threads !! currentIndex of
    Nothing -> throwError $ ErrorMessage "empty thread queue"
    Just (Tuple id plan) -> do
      plan' <- execute plan
      -- Threads could have changed, refresh.
      {threads: threads', currentIndex: currentIndex', newId} <- unwrap <$> get
      case updateAt currentIndex' (Tuple id plan') threads' of
        Nothing -> throwError $ ErrorMessage "thread index out of bounds"
        Just threads'' ->
          put $ flip (creepState threads'') newId $
            (currentIndex' + 1) `mod` length threads''

getThreadCount :: forall t m. MonadState (CreepState t) m => m Int
getThreadCount = length <<< (_.threads) <<< unwrap <$> get

creepState ::
  forall t. Array (Tuple ThreadId t) -> Int -> ThreadId -> CreepState t
creepState threads currentIndex newId =
  CreepState {threads, currentIndex, newId}
