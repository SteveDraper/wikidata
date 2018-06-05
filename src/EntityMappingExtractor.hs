{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module EntityMappingExtractor where

import Data.Text
import Data.Maybe
import Data.Tuple
import Data.Aeson
import Control.Lens
import Control.Lens.Traversal
import Data.Aeson.Lens
import Data.Map.Lens
import Conduit
import qualified Data.Conduit.Combinators as CC

import EntityMapping
import CoreTypes
import DataStream
import Wiki
import Extract
import Routable

entityMappingExtractor :: Monad m => Extractor m
entityMappingExtractor = mapC fromWiki .|
                  filterJust .|
                  mapC RoutableEntityMapping where
  fromWiki WikiRecord{..} = EntityMapping <$> idValue <*> wikiRef where
    wikiRef = fmap (WikiRef . WikiTitle) $ body ^? key "sitelinks" . key "enwiki" .key "title" . _String
    idValue = fmap EntityId $ body ^? key "id" . _String

hasWikiRef :: WikiRecord -> Bool
hasWikiRef WikiRecord{..} = isJust $ body ^? key "sitelinks" . key "enwiki"