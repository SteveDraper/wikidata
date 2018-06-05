module Filter where

import Data.Semigroup
import Data.Functor.Contravariant

data AllFilter a = AllFilter (a -> Bool)

instance Semigroup (AllFilter a) where
  (AllFilter f1) <> (AllFilter f2) = AllFilter $ fmap getAll $ (All . f1) <> (All . f2)

instance Contravariant AllFilter where
  contramap f (AllFilter g) = AllFilter (g . f)

apply :: AllFilter a -> a -> Bool
apply (AllFilter f) = f

