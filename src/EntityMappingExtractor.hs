{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module EntityMappingExtractor where

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

hasWikiRef :: KatipContext m => WikiRecord -> m Bool
hasWikiRef WikiRecord{..} = logIfFalse logNoRef value where
  value = isJust $ body ^? key "sitelinks" . key "enwiki"
  logIfFalse log result = (if result then return () else log) >> return value
  logNoRef = logFM DebugS $ logStr noRefMessage
  noRefMessage = "No English wiki-ref for " <> id
  id = fromMaybe "<Unknown" $ body ^? key "id" . _String
