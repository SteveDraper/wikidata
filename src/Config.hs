module Config where

import FileSpec
import Claim
import Extractions
import Compress
import Format
import Logging

data Config = Config {
  source :: InputFile,
  maxRecords :: Maybe Int,
  claimFilter :: Maybe ClaimFilter,
  outdir :: String,
  claimsFile :: OutputFile,
  entityMappingsFile :: OutputFile,
  labelsFile :: OutputFile,
  extractions :: ExtractionSet,
  compressFlag :: Compress,
  format :: Format,
  logLevel :: LogLevel,
  noOutput :: Bool
} deriving Show