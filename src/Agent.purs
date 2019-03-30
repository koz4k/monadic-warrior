module Agent (assignPlan, getAgentMemory, hasPlan, runAgent, setAgentMemory, module Agent.Class) where

import Agent.Class (class Agent, ActionResult(..), AgentName, AgentType, active, agents, asAgentName, asAgentType, executeAction, name, type_)
import Control.Monad.Except (class MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (execStateT)
import Control.Monad.Simplifiable (simplify)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either, either, isRight)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Error (mapError)
import Memory (getNestedMemory3, setNestedMemory3)
import Plan (Plan, executePlan)
import Prelude (Unit, bind, flip, map, pure, show, when, ($), (<<<), (<>))
import Threads (Threads, initThreads, runThreads)
import Type.Proxy (Proxy(..))

assignPlan ::
  forall e m ac ag m'.
    Agent e m ac ag => MonadEffect m' =>
      ag -> Plan ac Unit -> m' Unit
assignPlan agent = setThreads agent <<< initThreads

hasPlan ::
  forall e m ac ag m'.
    Agent e m ac ag => MonadEffect m' => ag -> m' Boolean
hasPlan agent = do
  map isRight $ runExceptT $ getThreads agent

runAgent ::
  forall e m ac ag. 
    Agent e m ac ag => MonadRec m => MonadEffect m =>
      ag -> ExceptT String Effect Unit
runAgent agent = when (active agent) do
  state <- getThreads agent
  state' <-
    mapError describeAgent $ simplify $ flip execStateT state $ runThreads $
      executePlan agent
  setThreads agent state'
  where
    describeAgent error =
      "error in " <> show (type_ :: AgentType ag) <> " " <>
        show (name agent) <> ": " <> error

setThreads ::
  forall e m ac ag m'.
    Agent e m ac ag => MonadEffect m' =>
      ag -> Threads (Plan ac Unit) -> m' Unit
setThreads agent = setAgentMemory agent "threads"

getThreads ::
  forall e m ac ag m'.
    Agent e m ac ag => MonadError String m' => MonadEffect m' =>
      ag -> m' (Threads (Plan ac Unit))
getThreads agent = do
  eitherState <- getAgentMemory agent "threads"
  either throwError pure eitherState

getAgentMemory ::
  forall e m ac ag m' a.
    Agent e m ac ag => DecodeJson a => MonadEffect m' =>
      ag -> String -> m' (Either String a)
getAgentMemory agent =
  liftEffect <<< getNestedMemory3 (agentMemoryRoot (Proxy :: Proxy ag))
                                  (show (name  agent :: AgentName ag))
                                  

setAgentMemory ::
  forall e m ac ag m' a.
    Agent e m ac ag => EncodeJson a => MonadEffect m' =>
      ag -> String -> a -> m' Unit
setAgentMemory agent key =
  liftEffect <<< setNestedMemory3 (agentMemoryRoot (Proxy :: Proxy ag))
                                  (show (name agent :: AgentName ag))
                                  key

agentMemoryRoot :: forall e m ac ag. Agent e m ac ag => Proxy ag -> String
agentMemoryRoot _ = show (type_ :: AgentType ag) <> "s"
