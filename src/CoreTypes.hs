module CoreTypes where

import Data.Text
import Control.Lens

newtype EntityId = EntityId Text deriving (Show, Eq, Ord)
newtype RelId = RelId Text deriving (Show, Eq, Ord)

newtype WikiTitle = WikiTitle Text deriving Show

data WikiRef = WikiRef WikiTitle deriving Show

wikiRef :: Lens' WikiRef Text
wikiRef = lens get set where
  get (WikiRef (WikiTitle t)) = t
  set _ t = WikiRef $ WikiTitle t

entityId :: Lens' EntityId Text
entityId = lens get set where
  get (EntityId t) = t
  set _ t = EntityId t

relId :: Lens' RelId Text
relId = lens get set where
  get (RelId t) = t
  set _ t = RelId t