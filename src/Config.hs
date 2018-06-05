module Config where

import FileSpec
import Claim
import Extractions
import Compress
import Format

data Config = Config {
  source :: InputFile,
  maxRecords :: Maybe Int,
  claimFilter :: Maybe ClaimFilter,
  outdir :: String,
  claimsFile :: OutputFile,
  entityMappingsFile :: OutputFile,
  extractions :: ExtractionSet,
  compressFlag :: Compress,
  format :: Format
} deriving Show