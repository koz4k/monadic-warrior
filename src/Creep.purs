module Creep (assignPlan, hasPlan, runCreep) where

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Except (class MonadError, throwError)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Holder (runHolderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (execStateT)
import Creep.Exec (ExecError(BadReturnCode, ErrorMessage))
import Creep.Plan (CreepPlan, executePlan)
import Creep.State (CreepState, initState, runThreads)
import Data.Bifunctor (lmap)
import Data.Either (either, isRight)
import Prelude (Unit, bind, flip, not, pure, show, when, ($), (<<<), (<=<), (<>))
import Screeps (CMD, Creep, MEMORY, TICK)
import Screeps.Creep (getMemory, name, setMemory, spawning)

assignPlan ::
  forall e m. MonadEff (memory :: MEMORY | e) m => Creep -> CreepPlan Unit -> m Unit
assignPlan creep = setState creep <<< initState

hasPlan :: forall e m. MonadEff (memory :: MEMORY | e) m => Creep -> m Boolean
hasPlan creep = do
  e <- runExceptT $ getState creep
  pure $ isRight e

runCreep ::
  forall e m.
    MonadRec m => MonadError String m =>
    MonadEff ( cmd :: CMD
             , memory :: MEMORY
             , random :: RANDOM
             , tick :: TICK
             | e
             ) m =>
      Creep -> m Unit
runCreep creep = when (not $ spawning creep) do
  state <- getState creep
  state' <-
    renderError $ flip execStateT state $ runHolderT $ runThreads $
      executePlan creep
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

setState ::
  forall e m.
    MonadEff (memory :: MEMORY | e) m =>
      Creep -> CreepState (CreepPlan Unit) -> m Unit
setState creep = liftEff <<< flip setMemory "state" creep

getState ::
  forall e m.
    MonadError String m =>
    MonadEff (memory :: MEMORY | e) m =>
      Creep -> m (CreepState (CreepPlan Unit))
getState creep = do
  eitherState <- liftEff $ getMemory creep "state"
  either throwError pure eitherState
