module Memory (getNestedMemory3, setNestedMemory3) where

import Effect (Effect)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either)
import Prelude (Unit, pure, (<<<))
import Screeps.Memory (getObjectMemory, setObjectMemory)

getNestedMemory3 ::
  forall a.
    DecodeJson a =>
      String -> String -> String -> Effect (Either String a)
getNestedMemory3 key1 key2 = pure <<< decodeJson <<< getObjectMemory key1 key2

setNestedMemory3 ::
  forall a.
    EncodeJson a =>
      String -> String -> String -> a -> Effect Unit
setNestedMemory3 key1 key2 key3 =
  setObjectMemory key1 key2 key3 <<< encodeJson
