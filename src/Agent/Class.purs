module Agent.Class (class Agent, ActionResult(..), AgentName, AgentType, active, agents, asAgentName, asAgentType, executeAction, name, type_) where

import Action (class Action)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT)
import Control.Monad.Translatable (class MonadTranslatable)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Error (class ErrorMessage)
import Prelude (class Show)

newtype AgentType ag = AgentType String

asAgentType :: forall ag. String -> AgentType ag
asAgentType = AgentType

instance showAgentType :: Show (AgentType ag) where
  show (AgentType t) = t

newtype AgentName ag = AgentName String

asAgentName :: forall ag. String -> AgentName ag
asAgentName = AgentName

instance showAgentName :: Show (AgentName ag) where
  show (AgentName t) = t

data ActionResult
  = Stay
  | Transition

class (ErrorMessage e, MonadThrow e m,
       MonadTranslatable m (ExceptT String Effect),
       Action ac) <= Agent e m ac ag | ag -> ac m, m -> e where
  type_ :: AgentType ag
  name :: ag -> AgentName ag
  agents :: forall m'. MonadEffect m' => m' (Array ag)
  active :: ag -> Boolean
  executeAction :: ag -> ac -> m ActionResult
