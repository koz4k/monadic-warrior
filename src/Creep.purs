module Creep (assignPlan, hasPlan, runCreep) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Except (class MonadError, ExceptT, throwError)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.State (execStateT)
import Control.Monad.Translatable (translate)
import Creep.Plan (CreepAgent(..), CreepPlan)
import Data.Either (either, isRight)
import Error (mapError)
import Plan (executePlan)
import Prelude (Unit, bind, flip, id, not, pure, show, when, ($), (<<<), (<>))
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

type CreepEff e
  = ( cmd    :: CMD
    , memory :: MEMORY
    , random :: RANDOM
    , tick   :: TICK
    | e
    )

runCreep :: forall e. Creep -> ExceptT String (Eff (CreepEff e)) Unit
runCreep creep = when (not $ spawning creep) do
  state <- getThreads creep
  state' <-
    mapError describeCreep $ translate $ setEff $ flip execStateT state $
      runThreads $ executePlan (CreepAgent creep)
  setThreads creep state'
  where
    describeCreep error =
      "error in creep " <> (show $ name creep) <> ": " <> error
    setEff :: forall m a. MonadEff (CreepEff e) m => m a -> m a
    setEff = id

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
