module Creep.Plan (Plan, executePlan, fork, harvestEnergy, interrupt, join, kill, plan, repeat, transferEnergyToBase) where

import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Except (class MonadError)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.Free (Free, liftF, resume, runFreeM, wrap)
import Control.Monad.State (class MonadState, State, evalState, get, modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (class MonadWriter, WriterT, execWriter, execWriterT, tell)
import Creep.Exec (Exec, ExecError(ErrorMessage), catchReturnCode, harvestSource, moveTo, transferToStructure)
import Creep.State (CreepState, ThreadId, addThread, hasThread, initThreadId, nextThreadId, removeThread)
import Data.Argonaut.Core (Json, JObject, foldJsonArray, foldJsonObject, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Array (head, singleton)
import Data.Either (Either(Right, Left), either, isRight)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse_)
import Monoid (Traversal(..))
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, Unit, bind, discard, flip, map, pure, unit, unless, when, ($), (*>), (<), (<$>), (<*>), (<<<), (<=<), (<>), (=<<), (>), (>>=), (>>>))
import Screeps (CMD, Creep, MEMORY, TICK, TargetPosition(..))
import Screeps.Creep (amtCarrying, carryCapacity)
import Screeps.FindType (find_my_spawns, find_sources)
import Screeps.Resource (resource_energy)
import Screeps.ReturnCode (err_not_in_range)
import Screeps.Room (find)
import Screeps.RoomObject (room)

data PlanF a
  = HarvestEnergy a
  | TransferEnergyToBase a
  -- Repeat takes "next" so that interpreting the free monad can terminate.
  -- TODO: Take a proof of a ~ Unit instead.
  | Repeat (Plan Unit) a
  | Interrupt (Plan Unit) (Plan Unit) a
  | Fork (Plan Unit) ThreadId a
  | Join ThreadId a
  | Kill ThreadId a

instance functorPlanF :: Functor PlanF where
  map k f = case f of
    HarvestEnergy f'        -> HarvestEnergy $ k f'
    TransferEnergyToBase f' -> TransferEnergyToBase $ k f'
    Repeat x f'             -> Repeat x $ k f'
    Interrupt x y f'        -> Interrupt x y $ k f'
    Fork x i f'             -> Fork x i $ k f'
    Join i f'               -> Join i $ k f'
    Kill i f'               -> Kill i $ k f'

newtype Plan a = Plan (Free PlanF a)

derive instance newtypePlan :: Newtype (Plan a) _

derive newtype instance functorPlan :: Functor Plan
derive newtype instance applyPlan :: Apply Plan
derive newtype instance applicativePlan :: Applicative Plan
derive newtype instance bindPlan :: Bind Plan
derive newtype instance monadPlan :: Monad Plan

instance encodeJsonPlan :: EncodeJson (Plan Unit) where
  encodeJson plan' =
    encodeJson $ execWriter $
      flip runFreeM (unwrap plan') $ flip (*>) <$> peelPure <*> case _ of
        HarvestEnergy _ -> do
          tellObject  $ "action" := "harvestEnergy"
                     ~> jsonEmptyObject
        TransferEnergyToBase _ -> do
          tellObject  $ "action" := "transferEnergyToBase"
                     ~> jsonEmptyObject
        Repeat block _ -> do
          tellObject  $ "action" := "repeat"
                     ~> "block"  := encodeJson block
                     ~> jsonEmptyObject
        Interrupt interruptee interrupter _ -> do
          tellObject  $ "action"      := "interrupt"
                     ~> "interruptee" := encodeJson interruptee
                     ~> "interrupter" := encodeJson interrupter
                     ~> jsonEmptyObject
        Fork thread threadId _ -> do
          tellObject  $ "action"   := "fork"
                     ~> "thread"   := encodeJson thread
                     ~> "threadId" := encodeJson threadId
                     ~> jsonEmptyObject
        Join threadId _ -> do
          tellObject  $ "action"   := "join"
                     ~> "threadId" := encodeJson threadId
                     ~> jsonEmptyObject
        Kill threadId _ -> do
          tellObject  $ "action"   := "kill"
                     ~> "threadId" := encodeJson threadId
                     ~> jsonEmptyObject
      where
        tellObject = tell <<< singleton

instance decodeJsonPlan :: DecodeJson (Plan Unit) where
  decodeJson = foldJsonArray (throwError "expected an array") decodeFromArray
    where
      decodeFromArray :: Array Json -> Either String (Plan Unit)
      decodeFromArray = map unwrap <<< execWriterT <<< traverse_ decodeAction
      decodeAction :: Json -> WriterT (Traversal Plan) (Either String) Unit
      decodeAction =
        foldJsonObject (throwError "expected an object") \object ->
          (lift $ object .? "action") >>= case _ of
            "harvestEnergy" -> tellAction $ HarvestEnergy unit
            "transferEnergyToBase" -> tellAction $ TransferEnergyToBase unit
            "repeat" ->
              tellAction <<< flip Repeat unit =<< decodeField object "block"
            "interrupt" ->
              tellAction =<< Interrupt <$> decodeField object "interruptee"
                                       <*> decodeField object "interrupter"
                                       <*> pure unit
            "fork" -> tellAction =<< Fork <$> decodeField object "thread"
                                          <*> decodeField object "threadId"
                                          <*> pure unit
            "join" ->
              tellAction <<< flip Join unit =<< decodeField object "threadId"
            "kill" ->
              tellAction <<< flip Kill unit =<< decodeField object "threadId"
            cmd -> throwError $ "unrecognized command: " <> cmd
      decodeField :: forall m a. MonadError String m => DecodeJson a => JObject -> String -> m a
      decodeField object field =
        either throwError pure $ decodeJson =<< (object .? field)
      --localDecodePlan :: forall m. MonadError String m => JObject -> String -> WriterT (Traversal Plan) m (Plan Unit)
      -- localPlan returns unit
      --localDecodePlan object = localPlan <<< decodeField object

peelPure ::
  forall m a. Applicative m => PlanF (Free PlanF a) -> m (Free PlanF a)
peelPure = case _ of
  HarvestEnergy next -> pure next
  TransferEnergyToBase next -> pure next
  Repeat block next -> pure next
  Interrupt interruptee interrupter next -> pure next
  Fork thread threadId next -> pure next
  Join threadId next -> pure next
  Kill threadId next -> pure next

type PlanM = WriterT (Traversal Plan) (State ThreadId)

plan :: PlanM Unit -> Plan Unit
plan = unwrap <<< flip evalState (nextThreadId initThreadId) <<< execWriterT

harvestEnergy :: PlanM Unit
harvestEnergy = tellAction $ HarvestEnergy unit

transferEnergyToBase :: PlanM Unit
transferEnergyToBase = tellAction $ TransferEnergyToBase unit

repeat :: PlanM Unit -> PlanM Unit
repeat = tellAction <<< flip Repeat unit <=< localPlan

interrupt :: PlanM Unit -> PlanM Unit -> PlanM Unit
interrupt interrupted interruptee =
  tellAction =<< Interrupt <$> localPlan interrupted
                           <*> localPlan interruptee
                           <*> pure unit

-- TODO: Phantom type for ThreadId.
fork :: PlanM Unit -> PlanM ThreadId
fork thread = do
  threadId <- get
  tellAction =<< Fork <$> localPlan thread <*> pure threadId <*> pure unit
  modify nextThreadId
  pure threadId
  
join :: ThreadId -> PlanM Unit
join = tellAction <<< flip Join unit

kill :: ThreadId -> PlanM Unit
kill = tellAction <<< flip Kill unit

tellAction :: forall m. MonadWriter (Traversal Plan) m => PlanF Unit -> m Unit
tellAction = tell <<< Traversal <<< Plan <<< liftF

localPlan ::
  forall m.
    Monad m =>
      WriterT (Traversal Plan) m Unit -> WriterT (Traversal Plan) m (Plan Unit)
localPlan = map unwrap <<< lift <<< execWriterT

executePlan ::
  forall e m.
    MonadState (CreepState (Plan Unit)) m =>
    MonadError ExecError m =>
    MonadEff (cmd :: CMD, memory :: MEMORY, tick :: TICK | e) m =>
      Creep -> Plan Unit -> Exec m (Plan Unit)
executePlan creep = unwrap >>> resume >>> case _ of
  Left action -> peel action
  Right _ -> pure $ pure unit
  where
    peel action = case action of
      HarvestEnergy next -> do
        if amtCarrying creep resource_energy < carryCapacity creep
          -- TODO: Use findClosestByPath instead.
          then case head $ find (room creep) find_sources of
            Just source -> do
              harvestSource creep source
                `catchNotInRange` (moveTo creep $ TargetObj source)
              stay
            Nothing -> throwError $ ErrorMessage "source not found"
          else transition next
      TransferEnergyToBase next -> do
        if amtCarrying creep resource_energy > 0
          then case head $ find (room creep) find_my_spawns of
            Just spawn -> do
              transferToStructure creep spawn resource_energy
                `catchNotInRange` (moveTo creep $ TargetObj spawn)
              stay
            Nothing -> throwError $ ErrorMessage "spawn not found"
          else transition next
      Repeat block _ -> do
        -- Execute just the block to avoid infinite loops.
        block' <- executePlan creep block
        pure $ block' *> plan'
      Interrupt interruptee interrupter next -> do
        interrupter' <- executePlan creep interrupter
        if isPure interrupter'
          then do
            -- No interruption - continue running interruptee.
            interruptee' <- executePlan creep interruptee
            if isPure interruptee'
              then
                -- Interruptee finished - transition to the next action.
                transition next
              else
                -- Interruptee is still running - update it in the plan.
                pure $ Plan do
                  liftF $ Interrupt interruptee' interrupter unit
                  next
          else
            -- Interruption - switch to interrupter.
            pure $ interrupter' *> plan'
      Fork thread threadId next -> do
        isRunning <- hasThread threadId
        -- Do not throw an error if the thread is missing because an action that
        -- transited must be idempotent (it can be executed multiple times if
        -- one of the next actions blocks in the same tick).
        unless isRunning $ addThread threadId thread
        transition next
      Join threadId next -> do
        isRunning <- hasThread threadId
        -- If the thread is running, wait until it finishes.
        if isRunning then stay else transition next
      Kill threadId next -> do
        isRunning <- hasThread threadId
        -- Same as in Fork.
        when isRunning $ removeThread threadId
        transition next
      where
        plan' = Plan $ wrap action
        catchNotInRange = catchReturnCode err_not_in_range
        stay = pure $ plan'
        transition = executePlan creep <<< Plan
        isPure = isRight <<< resume <<< unwrap
