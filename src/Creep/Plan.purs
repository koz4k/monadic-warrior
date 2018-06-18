module Creep.Plan (CreepAction, CreepAgent(..), CreepPlan, build, fight, harvestEnergy, transferEnergyToBase, upgradeController) where

import Action (class Action)
import Agent (class Agent, ActionResult(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Except (ExceptT)
import Control.Monad.Holder (HolderT)
import Control.Monad.State (State)
import Control.Monad.Writer (WriterT)
import Data.Argonaut.Core (jsonNull)
import Data.Array (elem, head)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(..))
import Data.Monoid.Applicative (Traversal)
import Data.Tuple (Tuple(..))
import Error (throwErrorMessage)
import Exec (ExecError, catchReturnCode)
import Exec.Creep (ExecStatus)
import Exec.Creep (build, harvestSource, moveTo, rangedAttackCreep, transferToStructure, upgradeController) as Exec
import Plan (Plan, ThreadId, tellAction)
import Prelude (Unit, bind, discard, pure, ($), (&&), (*>), (<), (>))
import Screeps (CMD, Creep, MEMORY, TargetPosition(TargetObj))
import Screeps.Creep (amtCarrying, carryCapacity)
import Screeps.FindType (FindType, find_construction_sites, find_hostile_creeps, find_my_structures, find_sources)
import Screeps.Refillable (energy, energyCapacity, toRefillable)
import Screeps.Resource (resource_energy)
import Screeps.ReturnCode (err_not_in_range)
import Screeps.Room (controller, find')
import Screeps.RoomObject (class RoomObject, pos, room)
import Screeps.RoomPosition (FindContext(OfType), findClosestByPath)
import Screeps.Structure (structureType, structure_extension, structure_spawn)

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

newtype CreepAgent = CreepAgent Creep

type CreepAgentM e
  = HolderT ExecStatus
            (ExceptT ExecError (Eff (cmd :: CMD, memory :: MEMORY | e)))

instance agentCreepAgent ::
    Agent ExecError
          (cmd :: CMD, memory :: MEMORY | e)
          (HolderT ExecStatus
                   (ExceptT ExecError (Eff (cmd :: CMD, memory :: MEMORY | e)))
          )
          CreepAction
          CreepAgent
    where
  executeAction (CreepAgent creep) = case _ of
    Build -> do
      if amtCarrying creep resource_energy > 0
        then do
          maybeSite <- findClosest find_construction_sites
          case maybeSite of
            Just site -> do
              Exec.build creep site `orMoveTo` site
              stay
            Nothing -> transition
        else transition
    Fight -> do
      maybeEnemy <- findClosest find_hostile_creeps
      case maybeEnemy of
        Just enemy ->
          (Exec.rangedAttackCreep creep enemy *> transition)
            `catchNotInRange` transition
        Nothing -> transition
    HarvestEnergy -> do
      if amtCarrying creep resource_energy < carryCapacity creep
        then do
          maybeSource <- findClosest find_sources
          case maybeSource of
            Just source -> do
              Exec.harvestSource creep source `orMoveTo` source
              stay
            Nothing -> throwErrorMessage "source not found"
        else transition
    TransferEnergyToBase -> do
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
              throwErrorMessage "refillable structure not found"
        else transition
    UpgradeController -> do
      if amtCarrying creep resource_energy > 0
        then case controller $ room creep of
          Just ctrl -> do
            Exec.upgradeController creep ctrl `orMoveTo` ctrl
            stay
          Nothing -> throwErrorMessage "controller not found"
        else transition
    where
      stay = pure Stay
      transition = pure Transition
      findClosest :: forall a. FindType a -> CreepAgentM e (Maybe a)
      findClosest findType =
        case findClosestByPath (pos creep) $ OfType findType of
          Right object -> pure object
          Left error -> throwErrorMessage $ message error
      orMoveTo ::
        forall a. RoomObject a => CreepAgentM e Unit -> a -> CreepAgentM e Unit
      orMoveTo action' object =
        action' `catchNotInRange` (Exec.moveTo creep $ TargetObj object)
      catchNotInRange ::
        forall a. CreepAgentM e a -> CreepAgentM e a -> CreepAgentM e a
      catchNotInRange = catchReturnCode err_not_in_range
