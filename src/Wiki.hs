module Wiki where

import Data.Maybe
import Data.Aeson
import Data.ByteString
import DataStream
import Conduit

data WikiRecord = WikiRecord {
  body :: Value
}

toWikiRecord :: Monad m => DataStream Value WikiRecord m
toWikiRecord = mapC WikiRecord

