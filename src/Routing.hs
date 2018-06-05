module Routing where

import Conduit
import Data.Functor
import qualified Data.Conduit.Combinators as CC

import Routable
import DataStream

data Routed m = Routed (DataStream Routable () m) Routable
data Route m = Route (DataStream Routable Void m) (Routable -> Bool)

router :: (Foldable r, Monad m) => r (Route m) -> DataStream Routable Void m
router rs = void $ sequenceSinks destinations where
  destinations = foldMap toSelectiveSink rs
  toSelectiveSink (Route sink pred) = [CC.filter pred .| sink]