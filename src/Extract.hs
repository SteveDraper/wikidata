{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Extract where

import Data.Text
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding
import Data.Aeson.Encode.Pretty
import Data.Aeson
import Control.Lens
import Data.Aeson.Lens

import Routable
import Wiki
import DataStream

type Extractor m = DataStream WikiRecord Routable m

prettifyObject :: Maybe Object -> [Text]
prettifyObject (Just o) = prettify $ Object o
prettifyObject Nothing = []

prettify :: Value -> [Text]
prettify v = [(LT.toStrict . decodeUtf8 . encodePretty) v]