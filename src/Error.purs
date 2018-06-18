module Error (class ErrorMessage, errorMessage, throwErrorMessage) where

import Control.Monad.Except (class MonadError, throwError)
import Prelude (class Eq, id, (<<<))

class Eq e <= ErrorMessage e where
  errorMessage :: String -> e

instance errorMessageString :: ErrorMessage String where
  errorMessage = id

throwErrorMessage ::
  forall e m a. ErrorMessage e => MonadError e m => String -> m a
throwErrorMessage = throwError <<< errorMessage
