module Error (class ErrorMessage, errorMessage, throwErrorMessage) where

import Control.Monad.Except (class MonadError, throwError)
import Prelude (id, (<<<))

class ErrorMessage e where
  errorMessage :: String -> e

instance errorMessageString :: ErrorMessage String where
  errorMessage = id

throwErrorMessage ::
  forall e m a. ErrorMessage e => MonadError e m => String -> m a
throwErrorMessage = throwError <<< errorMessage
