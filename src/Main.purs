module Main where

import Effect (Effect)
import Effect.Console (log)
import Control.Monad.Except.Trans (runExceptT)
import Creep (assignPlan, hasPlan, runCreep)
import Creep.Plan (build, fight, harvestEnergy, transferEnergyToBase, upgradeController)
import Data.Either (Either(..), either)
import Data.Monoid ((<>))
import Data.Traversable (traverse)
import Plan (interleave, interrupt, plan, repeat)
import Prelude (Unit, bind, discard, not, pure, void, when, ($), (<<<), (<=<), (=<<))
import Screeps.Creep (getMemory)
import Screeps.Game (creeps)

main :: Effect Unit
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
    harvesterPlan = plan $ (repeat do
      harvestEnergy
      transferEnergyToBase `interrupt` build)
        `interleave` repeat fight
    upgraderPlan = plan $ repeat do
      harvestEnergy
      upgradeController
    handleError = either log pure <=< runExceptT
