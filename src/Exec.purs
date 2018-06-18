module Exec (class BadReturnCode, ExecError, badReturnCode, catchReturnCode, liftSubAction, throwBadReturnCode) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Except (class MonadError, catchJust, throwError)
import Control.Monad.Holder (class MonadHolder, reserve)
import Data.Maybe (Maybe(..))
import Error (class ErrorDetails, class ErrorMessage)
import Prelude (class Eq, Unit, bind, const, pure, show, unit, void, ($), (<<<), (<>), (==))
import Screeps.ReturnCode (ReturnCode, ok)

class Eq e <= BadReturnCode e where
  badReturnCode :: ReturnCode -> e

data ExecError
  = EErrorMessage String
  | EBadReturnCode ReturnCode

derive instance eqExecError :: Eq ExecError

instance errorDetailsExecError :: ErrorDetails ExecError where
  renderError = case _ of
    EErrorMessage  message -> message
    EBadReturnCode code    -> "code " <> show code

instance errorMessageExecError :: ErrorMessage ExecError where
  errorMessage = EErrorMessage

instance badReturnCodeExecError :: BadReturnCode ExecError where
  badReturnCode = EBadReturnCode

throwBadReturnCode ::
  forall e m a. BadReturnCode e => MonadError e m => ReturnCode -> m a
throwBadReturnCode = throwError <<< badReturnCode

liftSubAction ::
  forall r er ef m.
    MonadHolder r m => BadReturnCode er => MonadError er m => MonadEff ef m =>
      r -> Eff ef ReturnCode -> m Unit
liftSubAction status subAction =
  void $ reserve status do
    code <- liftEff subAction
    if code == ok
      then pure unit
      else throwBadReturnCode code

catchReturnCode ::
  forall e m a.
    BadReturnCode e => MonadError e m => ReturnCode -> m a -> m a -> m a
catchReturnCode code action handler = action `catchIt` const handler
  where
    catchIt = catchJust \error ->
      if error == badReturnCode code
        then Just error
        else Nothing
