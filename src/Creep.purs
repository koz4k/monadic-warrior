module Creep where

import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Either (isRight)
import Data.Functor ((<$>))
import Prelude (Unit, flip, ($), (<<<), (=<<))

import Screeps (CMD, Creep, MEMORY, TICK)
import Screeps.Creep (getMemory, setMemory)

import Creep.Plan (Plan, executePlan)

assignPlan :: forall e. Creep -> Plan Unit -> Eff (memory :: MEMORY | e) Unit
assignPlan = flip setMemory "plan"

getPlan :: forall e. Creep -> ExceptT String (Eff (memory :: MEMORY | e)) (Plan Unit)
getPlan creep = ExceptT $ getMemory creep "plan"

hasPlan :: forall e. Creep -> Eff (memory :: MEMORY | e) Boolean
hasPlan creep = isRight <$> runExceptT (getPlan creep)

runCreep ::
  forall e. Creep ->
            ExceptT String
                    (Eff (cmd :: CMD, memory :: MEMORY, tick :: TICK | e))
                    Unit
runCreep creep =
  lift <<< assignPlan creep =<< executePlan creep =<< getPlan creep
