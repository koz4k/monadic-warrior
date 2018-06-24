module Creep (CreepAction, CreepAgent, CreepPlan, build, fight, harvestEnergy, transferEnergyToBase, upgradeController) where

import Action (class Action)
import Agent (class Agent, ActionResult(..), asAgentName, asAgentType)
import Control.Monad.Except (ExceptT)
import Control.Monad.Holder (HolderT)
import Control.Monad.State (State)
import Control.Monad.Writer (WriterT)
import Data.Argonaut.Core (jsonNull)
import Data.Array (elem, fromFoldable, head)
import Data.Either (Either(Left, Right))
import Data.Map (values)
import Data.Maybe (Maybe(..))
import Data.Monoid.Applicative (Traversal)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Error (throwErrorMessage)
import Exec (ExecError, catchReturnCode)
import Exec.Creep (ExecStatus)
import Exec.Creep (build, harvestSource, moveTo, rangedAttackCreep, transferToStructure, upgradeController) as Exec
import Plan (Plan, ThreadId, tellAction)
import Prelude (Unit, bind, discard, map, not, pure, show, ($), (&&), (*>), (<), (<$>), (<<<), (>))
import Screeps (Creep, TargetPosition(TargetObj))
import Screeps.Creep (amtCarrying, carryCapacity, spawning)
import Screeps.Creep (name) as Creep
import Screeps.FindType (FindType, find_construction_sites, find_hostile_creeps, find_my_structures, find_sources)
import Screeps.Game (creeps)
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

type CreepAgentM
  = HolderT ExecStatus
            (ExceptT ExecError Effect)

instance agentCreepAgent ::
    Agent ExecError
          (HolderT ExecStatus (ExceptT ExecError Effect))
          CreepAction
          CreepAgent
    where
  type_ = asAgentType "creep"
  name (CreepAgent creep) = asAgentName $ show $ Creep.name creep
  agents = map CreepAgent <<< fromFoldable <<< values <$> liftEffect creeps
  active (CreepAgent creep) = not $ spawning creep
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
      findClosest :: forall a. FindType a -> CreepAgentM (Maybe a)
      findClosest findType =
        case findClosestByPath (pos creep) $ OfType findType of
          Right object -> pure object
          Left error -> throwErrorMessage $ message error
      orMoveTo ::
        forall a. RoomObject a => CreepAgentM Unit -> a -> CreepAgentM Unit
      orMoveTo action' object =
        action' `catchNotInRange` (Exec.moveTo creep $ TargetObj object)
      catchNotInRange ::
        forall a. CreepAgentM a -> CreepAgentM a -> CreepAgentM a
      catchNotInRange = catchReturnCode err_not_in_range
