module Creep (assignPlan, hasPlan, runCreep) where

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Except (class MonadError, throwError)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Holder (runHolderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (execStateT)
import Creep.Plan (CreepPlan, executePlan)
import Data.Bifunctor (lmap)
import Data.Either (either, isRight)
import Error (renderError)
import Prelude (Unit, bind, flip, not, pure, show, when, ($), (<<<), (<=<), (<>))
import Screeps (CMD, Creep, MEMORY, TICK)
import Screeps.Creep (getMemory, name, setMemory, spawning)
import Threads (Threads, initThreads, runThreads)

assignPlan ::
  forall e m. MonadEff (memory :: MEMORY | e) m => Creep -> CreepPlan Unit -> m Unit
assignPlan creep = setThreads creep <<< initThreads

hasPlan :: forall e m. MonadEff (memory :: MEMORY | e) m => Creep -> m Boolean
hasPlan creep = do
  e <- runExceptT $ getThreads creep
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
  state <- getThreads creep
  state' <-
    translateError $ flip execStateT state $ runHolderT $ runThreads $
      executePlan creep
  setThreads creep state'
  where
    translateError =
      either throwError pure <<< lmap renderMessage <=< runExceptT
      where
        renderMessage error =
          "error in creep " <> (show $ name creep) <> ": " <>
            renderError error

setThreads ::
  forall e m.
    MonadEff (memory :: MEMORY | e) m =>
      Creep -> Threads (CreepPlan Unit) -> m Unit
setThreads creep = liftEff <<< flip setMemory "threads" creep

getThreads ::
  forall e m.
    MonadError String m =>
    MonadEff (memory :: MEMORY | e) m =>
      Creep -> m (Threads (CreepPlan Unit))
getThreads creep = do
  eitherState <- liftEff $ getMemory creep "threads"
  either throwError pure eitherState
