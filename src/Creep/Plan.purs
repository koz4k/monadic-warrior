module Creep.Plan (Plan, PVar, ThreadId, build, executePlan, fight, fork, harvestEnergy, interleave, interrupt, join, kill, plan, repeat, transferEnergyToBase, upgradeController) where

import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Except (class MonadError)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.Free (Free, liftF, resume, runFreeM, wrap)
import Control.Monad.State (class MonadState, State, evalState, get, modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (class MonadWriter, WriterT, execWriter, execWriterT, tell)
import Creep.Exec (Exec, ExecError(ErrorMessage), catchReturnCode)
import Creep.Exec (build, harvestSource, moveTo, rangedAttackCreep, transferToStructure, upgradeController) as Exec
import Creep.State (CreepState, addThread, hasThread, removeThread)
import Data.Argonaut.Core (JObject, foldJsonArray, foldJsonObject, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Array (elem, head, singleton)
import Data.Either (Either(Right, Left), either, isRight)
import Data.Maybe (Maybe(..))
import Data.Monoid.Applicative (Traversal(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse_)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, Unit, bind, discard, flip, map, pure, unit, unless, when, ($), (&&), (*>), (+), (<), (<$>), (<*>), (<<<), (<=<), (<>), (=<<), (>), (>>=), (>>>))
import Screeps (CMD, Creep, MEMORY, TICK, TargetPosition(..))
import Screeps.Creep (amtCarrying, carryCapacity)
import Screeps.FindType (FindType, find_construction_sites, find_hostile_creeps, find_my_structures, find_sources)
import Screeps.Refillable (energy, energyCapacity, toRefillable)
import Screeps.Resource (resource_energy)
import Screeps.ReturnCode (err_not_in_range)
import Screeps.Room (controller, find')
import Screeps.RoomObject (class RoomObject, pos, room)
import Screeps.RoomPosition (FindContext(OfType), findClosestByPath)
import Screeps.Structure (structureType, structure_extension, structure_spawn)

data PlanF a
  = Build a
  | Fight a
  | HarvestEnergy a
  | TransferEnergyToBase a
  | UpgradeController a
  -- Repeat takes "next" so that interpreting the free monad can terminate.
  | Repeat (Plan Unit) a
  | Interrupt (Plan Unit) (Plan Unit) a
  | Fork (Plan Unit) Int a
  | Join Int a
  | Kill Int a

instance functorPlanF :: Functor PlanF where
  map k f = case f of
    Build f'                -> Build $ k f'
    Fight f'                -> Fight $ k f'
    HarvestEnergy f'        -> HarvestEnergy $ k f'
    TransferEnergyToBase f' -> TransferEnergyToBase $ k f'
    UpgradeController f'    -> UpgradeController $ k f'
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
        Build _ -> do
          tellObject  $ "action" := "build"
                     ~> jsonEmptyObject
        Fight _ -> do
          tellObject  $ "action" := "fight"
                     ~> jsonEmptyObject
        HarvestEnergy _ -> do
          tellObject  $ "action" := "harvestEnergy"
                     ~> jsonEmptyObject
        TransferEnergyToBase _ -> do
          tellObject  $ "action" := "transferEnergyToBase"
                     ~> jsonEmptyObject
        UpgradeController _ -> do
          tellObject  $ "action" := "upgradeController"
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
      decodeFromArray = map unwrap <<< execWriterT <<< traverse_ decodeAction
      decodeAction =
        foldJsonObject (throwError "expected an object") \object ->
          (lift $ object .? "action") >>= case _ of
            "build" -> tellAction $ Build unit
            "fight" -> tellAction $ Fight unit
            "harvestEnergy" -> tellAction $ HarvestEnergy unit
            "transferEnergyToBase" -> tellAction $ TransferEnergyToBase unit
            "upgradeController" -> tellAction $ UpgradeController unit
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
      decodeField ::
        forall m a.
          MonadError String m => DecodeJson a => JObject -> String -> m a
      decodeField object field =
        either throwError pure $ decodeJson =<< (object .? field)

peelPure ::
  forall m a. Applicative m => PlanF (Free PlanF a) -> m (Free PlanF a)
peelPure = case _ of
  Build next -> pure next
  Fight next -> pure next
  HarvestEnergy next -> pure next
  TransferEnergyToBase next -> pure next
  UpgradeController next -> pure next
  Repeat block next -> pure next
  Interrupt interruptee interrupter next -> pure next
  Fork thread threadId next -> pure next
  Join threadId next -> pure next
  Kill threadId next -> pure next

type PlanM = WriterT (Traversal Plan) (State Int)

newtype PVar a = PVar Int

data ThreadId

plan :: PlanM Unit -> Plan Unit
plan = unwrap <<< flip evalState 1 <<< execWriterT

build :: PlanM Unit
build = tellAction $ Build unit

fight :: PlanM Unit
fight = tellAction $ Fight unit

harvestEnergy :: PlanM Unit
harvestEnergy = tellAction $ HarvestEnergy unit

transferEnergyToBase :: PlanM Unit
transferEnergyToBase = tellAction $ TransferEnergyToBase unit

upgradeController :: PlanM Unit
upgradeController = tellAction $ UpgradeController unit

repeat :: PlanM Unit -> PlanM Unit
repeat = tellAction <<< flip Repeat unit <=< localPlan

interrupt :: PlanM Unit -> PlanM Unit -> PlanM Unit
interrupt interrupted interruptee =
  tellAction =<< Interrupt <$> localPlan interrupted
                           <*> localPlan interruptee
                           <*> pure unit

fork :: PlanM Unit -> PlanM (PVar ThreadId)
fork thread = do
  threadId <- get
  tellAction =<< Fork <$> localPlan thread <*> pure threadId <*> pure unit
  modify (_ + 1)
  pure $ PVar threadId
  
join :: PVar ThreadId -> PlanM Unit
join (PVar threadId) = tellAction $ Join threadId unit

kill :: PVar ThreadId -> PlanM Unit
kill (PVar threadId) = tellAction $ Kill threadId unit

interleave :: PlanM Unit -> PlanM Unit -> PlanM Unit
interleave interleaved interleavee = do
  threadId <- fork interleavee
  interleaved
  kill threadId

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
      Build next -> do
        if amtCarrying creep resource_energy > 0
          then do
            maybeSite <- findClosest find_construction_sites
            case maybeSite of
              Just site -> do
                Exec.build creep site `orMoveTo` site
                stay
              Nothing -> transition next
          else transition next
      Fight next -> do
        maybeEnemy <- findClosest find_hostile_creeps
        case maybeEnemy of
          Just enemy ->
            (Exec.rangedAttackCreep creep enemy *> stay)
              `catchNotInRange` transition next
          Nothing -> transition next
      HarvestEnergy next -> do
        if amtCarrying creep resource_energy < carryCapacity creep
          then do
            maybeSource <- findClosest find_sources
            case maybeSource of
              Just source -> do
                Exec.harvestSource creep source `orMoveTo` source
                stay
              Nothing -> throwError $ ErrorMessage "source not found"
          else transition next
      TransferEnergyToBase next -> do
        if amtCarrying creep resource_energy > 0
          then let
            structures = find' (room creep) find_my_structures \structure ->
              case toRefillable structure of
                Just structure' ->
                  energy structure' < energyCapacity structure' &&
                    structureType structure `elem`
                      [structure_spawn, structure_extension]
                Nothing -> false
            in case head structures of
              Just structure -> do
                Exec.transferToStructure creep structure resource_energy
                  `orMoveTo` structure
                stay
              Nothing ->
                throwError $ ErrorMessage "refillable structure not found"
          else transition next
      UpgradeController next -> do
        if amtCarrying creep resource_energy > 0
          then case controller $ room creep of
            Just ctrl -> do
              Exec.upgradeController creep ctrl `orMoveTo` ctrl
              stay
            Nothing -> throwError $ ErrorMessage "controller not found"
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
        findClosest :: forall a. FindType a -> Exec m (Maybe a)
        findClosest findType =
          case findClosestByPath (pos creep) $ OfType findType of
            Right object -> pure object
            Left error -> throwError $ ErrorMessage $ message error
        orMoveTo :: forall a. RoomObject a => Exec m Unit -> a -> Exec m Unit
        orMoveTo action' object =
          action' `catchNotInRange` (Exec.moveTo creep $ TargetObj object)
        catchNotInRange :: forall a. Exec m a -> Exec m a -> Exec m a
        catchNotInRange = catchReturnCode err_not_in_range
        stay = pure plan'
        transition = executePlan creep <<< Plan
        isPure = isRight <<< resume <<< unwrap
