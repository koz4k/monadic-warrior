module Main where

import Agent (agents, assignPlan, getAgentMemory, hasPlan, runAgent)
import Control.Monad.Except.Trans (runExceptT)
import Creep (CreepAgent, build, fight, harvestEnergy, transferEnergyToBase, upgradeController)
import Data.Either (Either(..), either)
import Data.Lazy (defer, force)
import Data.Monoid ((<>))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Plan (interleave, interrupt, plan, repeat)
import Prelude (Unit, bind, discard, not, pure, void, when, ($), (<<<), (<=<), (=<<))

main :: Effect Unit
main = do
  void $ traverse (assignPlanToNewCreep) =<< creeps
  void $ traverse (handleError <<< runAgent) =<< creeps
  where
    creeps :: Effect (Array CreepAgent)
    creeps = agents
    assignPlanToNewCreep creep = do
      creepHasPlan <- hasPlan creep
      when (not creepHasPlan) do
        roleOrError <- getAgentMemory creep "role"
        case roleOrError of
          Right "harvester" -> assignPlan creep $ force harvesterPlan
          Right "upgrader"  -> assignPlan creep $ force upgraderPlan
          Right role        -> log $ "unknown role " <> role
          Left error        -> log error
    harvesterPlan = defer \_ -> plan $ (repeat do
      harvestEnergy
      transferEnergyToBase `interrupt` build)
        `interleave` repeat fight
    upgraderPlan = defer \_ -> plan $ repeat do
      harvestEnergy
      upgradeController
    handleError = either log pure <=< runExceptT
