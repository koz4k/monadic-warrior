module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Except.Trans (runExceptT)
import Creep (assignPlan, hasPlan, runCreep)
import Creep.Plan (build, harvestEnergy, interrupt, plan, repeat, transferEnergyToBase, upgradeController)
import Data.Either (Either(..), either)
import Data.Monoid ((<>))
import Data.Traversable (traverse)
import Prelude (Unit, bind, discard, not, pure, void, when, ($), (<<<), (<=<), (=<<))
import Screeps (CMD, MEMORY, TICK)
import Screeps.Creep (getMemory)
import Screeps.Game (creeps)

main ::
  Eff ( cmd :: CMD
      , console :: CONSOLE
      , memory :: MEMORY
      , random :: RANDOM
      , tick :: TICK
      ) Unit
main = do
  void $ traverse (assignPlanToNewCreep) =<< creeps
  void $ traverse (handleError <<< runCreep) =<< creeps
  where
    assignPlanToNewCreep creep = do
      creepHasPlan <- hasPlan creep
      when (not creepHasPlan) do
        roleOrError <- getMemory creep "role"
        case roleOrError of
          Right "harvester" -> assignPlan creep harvesterPlan
          Right "upgrader"  -> assignPlan creep upgraderPlan
          Right role        -> log $ "unknown role " <> role
          Left error        -> log error
    harvesterPlan = plan $ repeat do
      harvestEnergy
      transferEnergyToBase `interrupt` build
    upgraderPlan = plan $ repeat do
      harvestEnergy
      upgradeController
    handleError = either log pure <=< runExceptT
