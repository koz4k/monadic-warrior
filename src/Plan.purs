module Plan (class Action, class MonadPlanBuilder, Plan(..), PlanF(..), PThreadId, ThreadId, decodeJsonAction, encodeJsonAction, interleave, interrupt, plan, repeat, tellAction) where

import Control.Monad.Builder (class MonadBuilder, create, localCreate)
import Control.Monad.Counter (class Counter, class MonadCounter, next)
import Control.Monad.Except (class MonadError, throwError)
import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (class MonadWriter, execWriter, execWriterT, tell)
import Data.Argonaut.Core (JObject, Json, foldJsonArray, foldJsonObject, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Array (singleton)
import Data.Default (class Default)
import Data.Either (Either, either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe, maybe)
import Data.Monoid.Applicative (Traversal(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(Tuple))
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, Unit, bind, discard, flip, id, map, pure, unit, ($), (+), (<$>), (<*>), (<<<), (<=<), (<>), (=<<), (>>=))

type PThreadId = Int

data PlanF ac a
  = PAction ac a
  | PRepeat (Plan ac Unit)
  | PInterrupt (Plan ac Unit) (Plan ac Unit) a
  | PFork (Plan ac Unit) PThreadId a
  | PJoin PThreadId a
  | PKill PThreadId a

newtype Plan ac a = Plan (Free (PlanF ac) a)

derive instance newtypePlan :: Newtype (Plan ac a) _
derive instance functorPlanF :: Functor (PlanF ac)

derive newtype instance functorPlan :: Functor (Plan ac)
derive newtype instance applyPlan :: Apply (Plan ac)
derive newtype instance applicativePlan :: Applicative (Plan ac)
derive newtype instance bindPlan :: Bind (Plan ac)
derive newtype instance monadPlan :: Monad (Plan ac)

class Action ac where
  encodeJsonAction :: ac -> Tuple String Json
  decodeJsonAction :: String -> Json -> Maybe (Either String ac)

instance encodeJsonPlan :: Action ac => EncodeJson (Plan ac Unit) where
  encodeJson plan' =
    encodeJson $ execWriter $ flip runFreeM (unwrap plan') $ case _ of
        PAction action next -> do
          let Tuple name params = encodeJsonAction action
          tellObject  $ "action" := name
                     ~> "params" := params
                     ~> jsonEmptyObject
          pure next
        PRepeat block -> do
          tellObject  $ "action" := "repeat"
                     ~> "block"  := encodeJson block
                     ~> jsonEmptyObject
          pure $ pure unit
        PInterrupt interruptee interrupter next -> do
          tellObject  $ "action"      := "interrupt"
                     ~> "interruptee" := encodeJson interruptee
                     ~> "interrupter" := encodeJson interrupter
                     ~> jsonEmptyObject
          pure next
        PFork thread threadId next -> do
          tellObject  $ "action"   := "fork"
                     ~> "thread"   := encodeJson thread
                     ~> "threadId" := encodeJson threadId
                     ~> jsonEmptyObject
          pure next
        PJoin threadId next -> do
          tellObject  $ "action"   := "join"
                     ~> "threadId" := encodeJson threadId
                     ~> jsonEmptyObject
          pure next
        PKill threadId next -> do
          tellObject  $ "action"   := "kill"
                     ~> "threadId" := encodeJson threadId
                     ~> jsonEmptyObject
          pure next
      where
        tellObject = tell <<< singleton

instance decodeJsonPlan :: Action ac => DecodeJson (Plan ac Unit) where
  decodeJson = foldJsonArray (throwError "expected an array") decodeFromArray
    where
      decodeFromArray = map unwrap <<< execWriterT <<< traverse_ decodeAction
      decodeAction =
        foldJsonObject (throwError "expected an object") \object ->
          (lift $ object .? "action") >>= case _ of
            "repeat" -> do
              tellPlan <<< PRepeat =<< decodeField object "block"
            "interrupt" ->
              tellPlan =<< PInterrupt <$> decodeField object "interruptee"
                                      <*> decodeField object "interrupter"
                                      <*> pure unit
            "fork" -> tellPlan =<< PFork <$> decodeField object "thread"
                                         <*> decodeField object "threadId"
                                         <*> pure unit
            "join" ->
              tellPlan <<< flip PJoin unit =<< decodeField object "threadId"
            "kill" ->
              tellPlan <<< flip PKill unit =<< decodeField object "threadId"
            cmd ->
              tellPlan <<< flip PAction unit =<< liftEither do
                params <- object .? "params"
                maybe (throwError $ "unrecognized command: " <> cmd) id $
                  decodeJsonAction cmd params
      decodeField ::
        forall m a.
          MonadError String m => DecodeJson a => JObject -> String -> m a
      decodeField object field = liftEither $ decodeJson =<< object .? field
      liftEither :: forall m a b. MonadError a m => Either a b -> m b
      liftEither = either throwError pure

newtype ThreadId = ThreadId PThreadId

derive instance newtypeThreadId :: Newtype ThreadId _

instance defaultThreadId :: Default ThreadId where
  def = ThreadId 1

instance counterThreadId :: Counter ThreadId where
  inc (ThreadId id) = ThreadId $ id + 1

class (Action ac, MonadBuilder (Traversal (Plan ac)) m) <= MonadPlanBuilder ac m
  | ac -> m, m -> ac

instance monadPlanBuilderMonadBuilder ::
  (Action ac, MonadBuilder (Traversal (Plan ac)) m) => MonadPlanBuilder ac m

plan :: forall ac m a. MonadPlanBuilder ac m => m a -> Plan ac Unit
plan = unwrap <<< create

newtype PVar a = PVar a

repeat :: forall ac m. MonadPlanBuilder ac m => m Unit -> m Unit
repeat = tellPlan <<< PRepeat <=< localPlan

interrupt :: forall ac m. MonadPlanBuilder ac m => m Unit -> m Unit -> m Unit
interrupt interrupted interruptee =
  tellPlan =<< PInterrupt <$> localPlan interrupted
                          <*> localPlan interruptee
                          <*> pure unit

fork ::
  forall ac m.
    MonadCounter ThreadId m => MonadPlanBuilder ac m =>
      m Unit -> m (PVar ThreadId)
fork thread = do
  threadId <- next
  tellPlan =<< PFork <$> localPlan thread
                     <*> pure (unwrap threadId)
                     <*> pure unit
  pure $ PVar threadId

join :: forall ac m. MonadPlanBuilder ac m => PVar ThreadId -> m Unit
join (PVar (ThreadId threadId)) = tellPlan $ PJoin threadId unit

kill :: forall ac m. MonadPlanBuilder ac m => PVar ThreadId -> m Unit
kill (PVar (ThreadId threadId)) = tellPlan $ PKill threadId unit

interleave ::
  forall ac m.
    MonadCounter ThreadId m => MonadPlanBuilder ac m =>
      m Unit -> m Unit -> m Unit
interleave interleaved interleavee = do
  threadId <- fork interleavee
  interleaved
  kill threadId

tellAction :: forall m ac. MonadPlanBuilder ac m => ac -> m Unit
tellAction = tellPlan <<< flip PAction unit

tellPlan ::
  forall m ac. MonadWriter (Traversal (Plan ac)) m => PlanF ac Unit -> m Unit
tellPlan = tell <<< Traversal <<< Plan <<< liftF

localPlan :: forall m ac a. MonadPlanBuilder ac m => m a -> m (Plan ac Unit)
localPlan = map unwrap <<< localCreate
