{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module LabelExtractor where

import Data.Text
import Data.Maybe
import Data.Tuple
import Data.Semigroup
import Data.Aeson
import Control.Lens
import Control.Lens.Traversal
import Data.Aeson.Lens
import Data.Map.Lens
import Conduit
import qualified Data.Conduit.Combinators as CC
import Katip

import Label
import CoreTypes
import DataStream
import Wiki
import Extract
import Routable

labelExtractor :: Monad m => Extractor m
labelExtractor = mapC fromWiki .|
                  filterJust .|
                  mapC RoutableLabel where
  fromWiki WikiRecord{..} = Label <$> idValue <*> labelValue where
    labelValue = body ^? key "labels" . key "en" .key "value" . _String
    idValue = fmap EntityId $ body ^? key "id" . _String
