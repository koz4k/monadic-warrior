module Error (class ErrorDetails, class ErrorMessage, errorMessage, renderError, throwErrorMessage) where

import Control.Monad.Except (class MonadError, throwError)
import Prelude (class Eq, id, (<<<))

class Eq e <= ErrorDetails e where
  renderError :: e -> String

class ErrorDetails e <= ErrorMessage e where
  errorMessage :: String -> e

instance errorDetailsString :: ErrorDetails String where
  renderError = id

instance errorMessageString :: ErrorMessage String where
  errorMessage = id

throwErrorMessage ::
  forall e m a. ErrorMessage e => MonadError e m => String -> m a
throwErrorMessage = throwError <<< errorMessage
