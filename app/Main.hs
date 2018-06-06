{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Lens
import Control.Exception
import Conduit
import qualified Data.Conduit.Combinators as CC
import System.FilePath.Posix
import Control.Monad.State
import Katip
import System.IO
import Data.Text
import Data.ByteString

import Config
import FileSpec
import Options.Applicative
import Data.Semigroup ((<>))
import Wiki
import Write
import Claim
import ClaimExtractor
import EntityMapping
import EntityMappingExtractor
import Routable
import Routing
import DataStream
import JsonStream
import Filter
import Extractions
import Progress
import Compress
import Format
import Logging


instance PrimMonad m => PrimMonad (KatipContextT m) where
  type PrimState (KatipContextT m) = PrimState m
  primitive = lift . primitive

config :: Parser Config
config = Config
      <$> option str
          ( long "file" <>
            short 'f' <>
            value StdIn <>
            metavar "INPUT_FILE" <>
            help "File to read from (or stdin if omitted)"
          )
      <*> (optional $ option auto
          ( long "records" <>
            short 'r' <>
            metavar "MAX_RECORDS" <>
            help "Max JSON records to read (default all)"
          ))
      <*> (optional $ option str
          ( long "claimFilter" <>
            metavar "CLAIM_FILTER" <>
            help "Space-separated list of retained relation ids (all if absent)"
          ))
      <*> option str
          ( long "outdir" <>
            short 'o' <>
            value "." <>
            metavar "OUTPUT_DIRECTORY" <>
            help "Where to place the ouput file(s).  Defaults to current directory"
          )
      <*> option str
          ( long "claimsDest" <>
            (value $ OutputFile $ "claims.rdf") <>
            metavar "OUTPUT_FILE" <>
            help "Filename for the extracted claims triples (defaults to 'claims.rdf')"
          )
      <*> option str
          ( long "mappingsDest" <>
            (value $ OutputFile $ "entityMappings.rdf") <>
            metavar "OUTPUT_FILE" <>
            help "Filename for the extracted entity mappings (defaults to 'entityMappings.rdf')"
          )
      <*> option str
          ( long "include" <>
            short 'i' <>
            value allExtractions <>
            metavar "INCLUDE_FLAGS" <>
            help "What to include as a string of flag characters. \
                  \ 'c' == claims, 'e' == entities. \
                  \ If absent all known extractions will be performed"
          )
      <*> flag Compression NoCompression
          ( long "noCompress" <>
            help "Do not compress"
          )
      <*> flag FormatRDF FormatSql
          ( long "sql" <>
            help "Format for insertion into SQL tables (default off)"
          )
      <*> option auto
          ( long "logLevel" <>
            (value $ Info) <>
            metavar "LOG_LEVEL" <>
            help "Logging level (Debug | Info | None).  Defaults to Info"
          )

main :: IO ()
main = execute =<< execParser opts where
          opts = info (config <**> helper)
           ( fullDesc
            <> progDesc "Process Wikidata dump JSON"
            <> header "wikidata - a processor for wikidata dump JSON" )
          execute :: Config -> IO ()
          execute c@Config{..} = do
            handleScribe <- mkHandleScribe ColorIfTerminal stdout (toLogSeverity logLevel) V0
            let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "wikidata" "production"

            bracket mkLogEnv closeScribes $ \le -> do
              runKatipContextT le (mempty :: LogContexts) mempty $ stream c
          stream :: Config -> KatipContextT IO ()
          stream c = runResourceT $
                    void $
                    runStateT (pipeline c) initialState
          pipeline c = runConduit $
                    (fromInput c) .|
                    toWikiRecord .|
                    logProgress 1000 .|
                    updateRecordsProcessed .|
                    resolvable .|
                    updateRecordsResolved .|
                    void (sequenceConduits extractors) .|
                    (required c) .|
                    router [claimRouting c, entityMappingRouting c]
          required Config{..} = CC.filter $ apply (isRequired extractions)
          resolvable = CC.filterM hasWikiRef


extractors :: Monad m => [DataStream WikiRecord Routable m]
extractors = [claimExtractor, entityMappingExtractor]

claimRouting :: (PrimMonad m, MonadResource m, MonadThrow m) => Config -> Route m
claimRouting Config{..} = Route (toText .| outputConverter compressFlag .| simpleWriter output) $ apply $ isClaim <> (included claimFilter) where
  toText = mapC getClaim .| filterJust .| formatClaim format
  getClaim r = r ^? toClaim
  included :: Maybe ClaimFilter -> AllFilter Routable
  included Nothing = AllFilter $ const True
  included (Just cf) = toAllFilter cf
  output = makeOutput outdir compressFlag format claimsFile

entityMappingRouting :: (PrimMonad m, MonadResource m, MonadThrow m) => Config -> Route m
entityMappingRouting Config{..} = Route (toText .| outputConverter compressFlag .| simpleWriter output) $ apply $ isEntityMapping where
  toText = mapC getMapping .| filterJust .| formatMapping format
  getMapping r = r ^? toEntityMapping
  output = makeOutput outdir compressFlag format entityMappingsFile

outputConverter :: (PrimMonad m, MonadThrow m) => Compress -> DataStream Text ByteString m
outputConverter c = maybeCompress c linesToBytes

makeOutput :: String -> Compress -> Format -> OutputFile -> OutputFile
makeOutput d = setExt where
  setExt NoCompression t = withType t
  setExt Compression t = (addExt "gc") . (withType t)
  withType FormatRDF = indir . (repExt "rdf")
  withType FormatSql = indir . (repExt "sql")
  indir (OutputFile f) = OutputFile $ d </> f
  indir StdOut = StdOut
  addExt _ StdOut = StdOut
  addExt ext (OutputFile f) = OutputFile $ addExtension f ext
  repExt _ StdOut = StdOut
  repExt ext (OutputFile f) = OutputFile $ replaceExtension f ext