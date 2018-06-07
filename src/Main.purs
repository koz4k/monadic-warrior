module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Except.Trans (runExceptT)
import Creep (assignPlan, hasPlan, runCreep)
import Creep.Plan (harvestEnergy, plan, repeat, transferEnergyToBase)
import Data.Either (either)
import Data.Traversable (traverse)
import Prelude (Unit, bind, discard, not, pure, void, when, ($), (<<<), (<=<), (=<<))
import Screeps (CMD, MEMORY, TICK)
import Screeps.Game (creeps)

main ::
  Eff ( cmd :: CMD
      , console :: CONSOLE
      , memory :: MEMORY
      , random :: RANDOM
      , tick :: TICK
      ) Unit
main = do
  void $ traverse assignPlanToNewCreep =<< creeps
  void $ traverse (handleError <<< runCreep) =<< creeps
  where
    assignPlanToNewCreep creep = do
      creepHasPlan <- hasPlan creep
      when (not creepHasPlan) do
        assignPlan creep harvesterPlan
    harvesterPlan = plan $ repeat do
      harvestEnergy
      transferEnergyToBase
    handleError = either log pure <=< runExceptT
