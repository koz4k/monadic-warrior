module Monoid (Traversal(..)) where

import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap, wrap)
import Prelude (class Applicative, class Apply, class Semigroup, Unit, pure, unit, ($), (*>))

newtype Traversal f = Traversal (f Unit)

derive instance newtypeTraversal :: Newtype (Traversal f) _

instance semigroupTraversal :: Apply f => Semigroup (Traversal f) where
  append x y = wrap $ unwrap x *> unwrap y

instance monoidTraversal :: Applicative f => Monoid (Traversal f) where
  mempty = wrap $ pure unit
