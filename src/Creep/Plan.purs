module Creep.Plan (CreepAction, CreepPlan, build, executePlan, fight, harvestEnergy, transferEnergyToBase, upgradeController) where

import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Except (class MonadError)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.Free (liftF, resume, wrap)
import Control.Monad.Holder (class MonadHolder)
import Control.Monad.State (class MonadState, State)
import Control.Monad.Writer (WriterT)
import Creep.Exec (ExecError(ErrorMessage), ExecStatus, catchReturnCode)
import Creep.Exec (build, harvestSource, moveTo, rangedAttackCreep, transferToStructure, upgradeController) as Exec
import Data.Argonaut.Core (jsonNull)
import Data.Array (elem, head)
import Data.Either (Either(Right, Left), isRight)
import Data.Maybe (Maybe(..))
import Data.Monoid.Applicative (Traversal)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Plan (class Action, Plan(Plan), PlanF(PKill, PJoin, PFork, PInterrupt, PRepeat, PAction), ThreadId, tellAction)
import Prelude (Unit, bind, discard, pure, unit, unless, when, ($), (&&), (*>), (<), (<<<), (>), (>>>))
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
import Threads (Threads, addThread, hasThread, removeThread)

data CreepAction
  = Build
  | Fight
  | HarvestEnergy
  | TransferEnergyToBase
  | UpgradeController

instance actionCreepAction :: Action CreepAction where
  decodeJsonAction action _ = case action of
    "build"                -> Just $ Right Build
    "fight"                -> Just $ Right Fight
    "harvestEnergy"        -> Just $ Right HarvestEnergy
    "transferEnergyToBase" -> Just $ Right TransferEnergyToBase
    "upgradeController"    -> Just $ Right UpgradeController
    _                      -> Nothing
  encodeJsonAction = case _ of
    Build                -> Tuple "build"                jsonNull
    Fight                -> Tuple "fight"                jsonNull
    HarvestEnergy        -> Tuple "harvestEnergy"        jsonNull
    TransferEnergyToBase -> Tuple "transferEnergyToBase" jsonNull
    UpgradeController    -> Tuple "upgradeController"    jsonNull

type CreepPlan = Plan CreepAction

type CreepPlanBuilder = WriterT (Traversal CreepPlan) (State ThreadId)

build :: CreepPlanBuilder Unit
build = tellAction Build

fight :: CreepPlanBuilder Unit
fight = tellAction Fight

harvestEnergy :: CreepPlanBuilder Unit
harvestEnergy = tellAction HarvestEnergy

transferEnergyToBase :: CreepPlanBuilder Unit
transferEnergyToBase = tellAction TransferEnergyToBase

upgradeController :: CreepPlanBuilder Unit
upgradeController = tellAction UpgradeController

executePlan ::
  forall e m.
    MonadHolder ExecStatus m => MonadState (Threads (CreepPlan Unit)) m =>
    MonadError ExecError m =>
    MonadEff (cmd :: CMD, memory :: MEMORY, tick :: TICK | e) m =>
      Creep -> CreepPlan Unit -> m (CreepPlan Unit)
executePlan creep = unwrap >>> resume >>> case _ of
  Left action -> peel action
  Right _ -> pure $ pure unit
  where
    peel action = case action of
      PAction Build next -> do
        if amtCarrying creep resource_energy > 0
          then do
            maybeSite <- findClosest find_construction_sites
            case maybeSite of
              Just site -> do
                Exec.build creep site `orMoveTo` site
                stay
              Nothing -> transition next
          else transition next
      PAction Fight next -> do
        maybeEnemy <- findClosest find_hostile_creeps
        case maybeEnemy of
          Just enemy ->
            (Exec.rangedAttackCreep creep enemy *> stay)
              `catchNotInRange` transition next
          Nothing -> transition next
      PAction HarvestEnergy next -> do
        if amtCarrying creep resource_energy < carryCapacity creep
          then do
            maybeSource <- findClosest find_sources
            case maybeSource of
              Just source -> do
                Exec.harvestSource creep source `orMoveTo` source
                stay
              Nothing -> throwError $ ErrorMessage "source not found"
          else transition next
      PAction TransferEnergyToBase next -> do
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
      PAction UpgradeController next -> do
        if amtCarrying creep resource_energy > 0
          then case controller $ room creep of
            Just ctrl -> do
              Exec.upgradeController creep ctrl `orMoveTo` ctrl
              stay
            Nothing -> throwError $ ErrorMessage "controller not found"
          else transition next
      PRepeat block -> do
        -- Execute just the block to avoid infinite loops.
        block' <- executePlan creep block
        pure $ block' *> plan'
      PInterrupt interruptee interrupter next -> do
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
        findClosest :: forall a. FindType a -> m (Maybe a)
        findClosest findType =
          case findClosestByPath (pos creep) $ OfType findType of
            Right object -> pure object
            Left error -> throwError $ ErrorMessage $ message error
        orMoveTo :: forall a. RoomObject a => m Unit -> a -> m Unit
        orMoveTo action' object =
          action' `catchNotInRange` (Exec.moveTo creep $ TargetObj object)
        catchNotInRange :: forall a. m a -> m a -> m a
        catchNotInRange = catchReturnCode err_not_in_range
        stay = pure plan'
        transition = executePlan creep <<< Plan
        isPure :: CreepPlan Unit -> Boolean
        isPure = isRight <<< resume <<< unwrap
