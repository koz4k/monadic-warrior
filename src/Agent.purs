module Agent (class Agent, ActionResult(..), executeAction) where

import Action (class Action)
import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT)
import Control.Monad.Translatable (class MonadTranslatable)
import Error (class ErrorMessage)

data ActionResult
  = Stay
  | Transition

class (ErrorMessage er, MonadThrow er m,
       MonadTranslatable m (ExceptT String (Eff ef)),
       Action ac) <= Agent er ef m ac ag | ag -> ac m ef er where
  executeAction :: ag -> ac -> m ActionResult
