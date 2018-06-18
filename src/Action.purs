module Action (class Action, decodeJsonAction, encodeJsonAction) where

import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

class Action ac where
  encodeJsonAction :: ac -> Tuple String Json
  decodeJsonAction :: String -> Json -> Maybe (Either String ac)
