{-# LANGUAGE RecordWildCards #-}

module JsonStream where

import Data.ByteString
import Data.Maybe
import Conduit
import Data.Conduit
import Data.Conduit.Binary as CB
import Data.Conduit.List as Cl
import qualified Data.Conduit.Text as Ct
import Data.Aeson
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS

import Config
import FileSpec
import System.IO
import Data.Text(Text)
import Data.Text.IO as DTIO
import DataStream

fromInput :: MonadResource m => Config -> DataStream () Value m
fromInput Config{..} = processSource where
  processSource = (openSource source) .|
                    CB.lines .|
                    mapC toJson .|
                    filterJust .|
                    filterStage
  filterStage = fromMaybe (mapC id) $ fmap takeC maxRecords
  openSource :: MonadResource m => InputFile -> ConduitT i ByteString m ()
  openSource StdIn = CB.sourceHandle stdin
  openSource (InputFile f) = CB.sourceFile f
  toJson :: ByteString -> Maybe Value
  toJson = decode . canonicalize
  canonicalize l = LBS.fromStrict $ fromMaybe l $ stripSuffix (Char8.singleton ',') l
