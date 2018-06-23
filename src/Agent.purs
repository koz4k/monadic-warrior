module Agent (class Agent, ActionResult(..), executeAction) where

import Action (class Action)
import Effect (Effect)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT)
import Control.Monad.Translatable (class MonadTranslatable)
import Error (class ErrorMessage)

data ActionResult
  = Stay
  | Transition

class (ErrorMessage e, MonadThrow e m,
       MonadTranslatable m (ExceptT String Effect),
       Action ac) <= Agent e m ac ag | ag -> ac m e where
  executeAction :: ag -> ac -> m ActionResult
