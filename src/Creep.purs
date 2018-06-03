module Creep (assignPlan, hasPlan, runCreep) where

import Blockable (runBlockableT)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Except (class MonadError, throwError)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (execStateT)
import Creep.Exec (ExecError(..))
import Creep.Plan (Plan, executePlan)
import Creep.State (CreepState, getThreadCount, initState, runThread)
import Data.Bifunctor (lmap)
import Data.Either (either, isRight)
import Prelude (Unit, bind, discard, flip, map, not, pure, show, unit, when, ($), (-), (<<<), (<=<), (<>), (==))
import Screeps (CMD, Creep, MEMORY, TICK)
import Screeps.Creep (getMemory, name, setMemory, spawning)

assignPlan ::
  forall e m. MonadEff (memory :: MEMORY | e) m => Creep -> Plan Unit -> m Unit
assignPlan creep = setState creep <<< initState

hasPlan :: forall e m. MonadEff (memory :: MEMORY | e) m => Creep -> m Boolean
hasPlan = map isRight <<< runExceptT <<< getState

runCreep ::
  forall e m.
    MonadRec m => MonadError String m =>
    MonadEff (cmd :: CMD, memory :: MEMORY, tick :: TICK | e) m =>
      Creep -> m Unit
runCreep creep = when (not $ spawning creep) do
  state <- getState creep
  state' <- renderError $ flip execStateT state $ runBlockableT runThreads
  setState creep state'
  where
    renderError =
      either throwError pure <<< lmap renderMessage <=< runExceptT
      where
        renderMessage error =
          "error in creep " <> (show $ name creep) <> ": " <>
            renderDetails error
        renderDetails details = case details of
          ErrorMessage message -> message
          BadReturnCode code -> show code
    runThreads = do
      threadCount <- getThreadCount
      flip tailRecM threadCount \i ->
        if i == 0
          then pure $ Done unit
          else do
            runThread $ executePlan creep
            pure $ Loop $ i - 1

setState ::
  forall e m.
    MonadEff (memory :: MEMORY | e) m =>
      Creep -> CreepState (Plan Unit) -> m Unit
setState creep = liftEff <<< flip setMemory "state" creep

getState ::
  forall e m.
    MonadError String m =>
    MonadEff (memory :: MEMORY | e) m =>
      Creep -> m (CreepState (Plan Unit))
getState creep = do
  eitherState <- liftEff $ getMemory creep "state"
  either throwError pure eitherState
