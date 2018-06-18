module Error (class ErrorDetails, class ErrorMessage, errorMessage, mapError, renderError, throwErrorMessage) where

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, mapExceptT, throwError)
import Data.Bifunctor (lmap)
import Prelude (class Eq, class Monad, id, map, ($), (<<<))

class Eq e <= ErrorDetails e where
  renderError :: e -> String

class ErrorDetails e <= ErrorMessage e where
  errorMessage :: String -> e

instance errorDetailsString :: ErrorDetails String where
  renderError = id

instance errorMessageString :: ErrorMessage String where
  errorMessage = id

throwErrorMessage ::
  forall e m a. ErrorMessage e => MonadThrow e m => String -> m a
throwErrorMessage = throwError <<< errorMessage

mapError ::
  forall e e' m a. Monad m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
mapError f = mapExceptT $ map $ lmap f
