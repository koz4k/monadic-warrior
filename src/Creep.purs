module Creep (assignPlan, hasPlan, runCreep) where

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Control.Monad.Except (class MonadError, ExceptT, throwError)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.State (execStateT)
import Control.Monad.Translatable (translate)
import Creep.Plan (CreepAgent(..), CreepPlan)
import Data.Either (either, isRight)
import Error (mapError)
import Plan (executePlan)
import Prelude (Unit, bind, flip, not, pure, show, when, ($), (<<<), (<>))
import Screeps (Creep)
import Screeps.Creep (getMemory, name, setMemory, spawning)
import Threads (Threads, initThreads, runThreads)

assignPlan ::
  forall m. MonadEffect m => Creep -> CreepPlan Unit -> m Unit
assignPlan creep = setThreads creep <<< initThreads

hasPlan :: forall m. MonadEffect m => Creep -> m Boolean
hasPlan creep = do
  e <- runExceptT $ getThreads creep
  pure $ isRight e

runCreep :: Creep -> ExceptT String Effect Unit
runCreep creep = when (not $ spawning creep) do
  state <- getThreads creep
  state' <-
    mapError describeCreep $ translate $ flip execStateT state $
      runThreads $ executePlan (CreepAgent creep)
  setThreads creep state'
  where
    describeCreep error =
      "error in creep " <> (show $ name creep) <> ": " <> error

setThreads ::
  forall m.
    MonadEffect m =>
      Creep -> Threads (CreepPlan Unit) -> m Unit
setThreads creep = liftEffect <<< flip setMemory "threads" creep

getThreads ::
  forall m.
    MonadError String m => MonadEffect m =>
      Creep -> m (Threads (CreepPlan Unit))
getThreads creep = do
  eitherState <- liftEffect $ getMemory creep "threads"
  either throwError pure eitherState
