module Control.Monad.Counter (class Counter, class MonadCounter, inc, next) where

import Control.Monad.State (class MonadState, get, put)
import Data.Default (class Default)
import Prelude (bind, discard, pure, ($))

class Default c <= Counter c where
  inc :: c -> c

class Counter c <= MonadCounter c m | m -> c where
  next :: m c

instance monadCounterMonadState ::
    (Counter c, MonadState c m) => MonadCounter c m where
  next = do
    counter <- get
    put $ inc counter
    pure counter
