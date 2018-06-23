module Plan (class MonadPlanBuilder, Plan, ThreadId, executePlan, interleave, interrupt, plan, repeat, tellAction) where

import Action (class Action, decodeJsonAction, encodeJsonAction)
import Agent (class Agent, ActionResult(..), executeAction)
import Control.Monad.Builder (class MonadBuilder, create, localCreate)
import Control.Monad.Counter (class Counter, class MonadCounter, next)
import Control.Monad.Except (class MonadError, throwError)
import Control.Monad.Free (Free, liftF, resume, runFreeM, wrap)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (class MonadWriter, execWriter, execWriterT, tell)
import Data.Argonaut.Core (Json, caseJsonArray, caseJsonObject, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Array (singleton)
import Data.Default (class Default)
import Data.Either (Either(..), either, isRight)
import Data.Foldable (traverse_)
import Data.Maybe (maybe)
import Data.Monoid.Applicative (Traversal(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(Tuple))
import Foreign.Object (Object)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, Unit, bind, discard, flip, identity, map, pure, unit, unless, when, ($), (*>), (+), (<$>), (<*>), (<<<), (<=<), (<>), (=<<), (>>=), (>>>))
import Threads (Threads, addThread, hasThread, removeThread)

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
  decodeJson = caseJsonArray (throwError "expected an array") decodeFromArray
    where
      decodeFromArray = map unwrap <<< execWriterT <<< traverse_ decodeAction
      decodeAction =
        caseJsonObject (throwError "expected an object") \object ->
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
                maybe (throwError $ "unrecognized command: " <> cmd) identity $
                  decodeJsonAction cmd params
      decodeField ::
        forall m a.
          MonadError String m => DecodeJson a => Object Json -> String -> m a
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

executePlan ::
  forall e m ac ag.
    Monad m => Agent e m ac ag =>
      ag -> Plan ac Unit -> StateT (Threads (Plan ac Unit)) m (Plan ac Unit)
executePlan agent = unwrap >>> resume >>> case _ of
  Left action -> peel action
  Right _ -> pure $ pure unit
  where
    peel action = case action of
      PAction action' next -> lift (executeAction agent action') >>= case _ of
        Stay       -> stay
        Transition -> transition next
      PRepeat block -> do
        -- Execute just the block to avoid infinite loops.
        block' <- executePlan agent block
        pure $ block' *> plan'
      PInterrupt interruptee interrupter next -> do
        interrupter' <- executePlan agent interrupter
        if isPure interrupter'
          then do
            -- No interruption - continue running interruptee.
            interruptee' <- executePlan agent interruptee
            if isPure interruptee'
              then
                -- Interruptee finished - transition to the next action.
                transition next
              else
                -- Interruptee is still running - update it in the plan.
                pure $ Plan do
                  liftF $ PInterrupt interruptee' interrupter unit
                  next
          else
            -- Interruption - switch to interrupter.
            pure $ interrupter' *> plan'
      PFork thread threadId next -> do
        isRunning <- hasThread threadId
        -- Do not throw an error if the thread is missing because an action that
        -- transited must be idempotent (it can be executed multiple times if
        -- one of the next actions blocks in the same tick).
        unless isRunning $ addThread threadId thread
        transition next
      PJoin threadId next -> do
        isRunning <- hasThread threadId
        -- If the thread is running, wait until it finishes.
        if isRunning then stay else transition next
      PKill threadId next -> do
        isRunning <- hasThread threadId
        -- Same as in Fork.
        when isRunning $ removeThread threadId
        transition next
      where
        plan' = Plan $ wrap action
        stay = pure plan'
        transition = executePlan agent <<< Plan
        isPure :: Plan ac Unit -> Boolean
        isPure = isRight <<< resume <<< unwrap
