module Creep where

import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Trans.Class (lift)
import Creep.Plan (Plan, ThreadQueue, executePlan)
import Data.Array (singleton)
import Data.Either (isRight)
import Prelude (Unit, flip, map, not, when, ($), (<<<), (=<<))
import Screeps (CMD, Creep, MEMORY, TICK)
import Screeps.Creep (getMemory, setMemory, spawning)

assignPlan :: forall e. Creep -> Plan Unit -> Eff (memory :: MEMORY | e) Unit
assignPlan creep = assignThreadQueue creep <<< singleton

hasPlan :: forall e. Creep -> Eff (memory :: MEMORY | e) Boolean
hasPlan = map isRight <<< runExceptT <<< getThreadQueue

runCreep ::
  forall e. Creep ->
            ExceptT String
                    (Eff (cmd :: CMD, memory :: MEMORY, tick :: TICK | e))
                    Unit
runCreep creep = when (not $ spawning creep) $
  lift <<< assignThreadQueue creep =<< executePlan creep =<<
    getThreadQueue creep

assignThreadQueue ::
  forall e. Creep -> ThreadQueue -> Eff (memory :: MEMORY | e) Unit
assignThreadQueue = flip setMemory "threadQueue"

getThreadQueue ::
  forall e. Creep -> ExceptT String (Eff (memory :: MEMORY | e)) ThreadQueue
getThreadQueue creep = ExceptT $ getMemory creep "threadQueue"
