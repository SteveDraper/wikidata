{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module JsonStream where

import Data.ByteString
import Data.Maybe
import Data.Semigroup
import Conduit
import Data.Conduit
import Data.Conduit.Binary as CB
import Data.Conduit.List as Cl
import qualified Data.Conduit.Text as Ct
import Data.Aeson
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import Katip

import Config
import FileSpec
import System.IO
import Data.Text(Text)
import Data.Text.Encoding
import Data.Text.IO as DTIO
import DataStream

fromInput :: (KatipContext m, MonadResource m) => Config -> DataStream () Value m
fromInput Config{..} = processSource where
  processSource = (openSource source) .|
                    CB.lines .|
                    mapMC toJson .|
                    filterJust .|
                    filterStage
  filterStage = fromMaybe (mapC id) $ fmap takeC maxRecords
  openSource :: MonadResource m => InputFile -> ConduitT i ByteString m ()
  openSource StdIn = CB.sourceHandle stdin
  openSource (InputFile f) = CB.sourceFile f
  toJson :: KatipContext m => ByteString -> m (Maybe Value)
  toJson l = do
    maybeJson <- return $ (decode . canonicalize) l
    let
      logAction = case maybeJson of
        Just _ -> return ()
        Nothing -> logFM WarningS $ logStr $ "Unparsable JSON: " <> decodeUtf8 l
    logAction >> return maybeJson
  canonicalize l = LBS.fromStrict $ fromMaybe l $ stripSuffix (Char8.singleton ',') l


