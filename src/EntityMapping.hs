{-# LANGUAGE OverloadedStrings #-}

module EntityMapping where

import Data.Text as T
import Data.String
import Control.Lens
import Conduit
import Network.URI.Encode

import CoreTypes
import Format
import DataStream
import Sql

data EntityMapping = EntityMapping EntityId WikiRef deriving Show

formatMapping :: Monad m => Format -> DataStream EntityMapping Text m
formatMapping FormatRDF = awaitForever convert where
  convert (EntityMapping eid wid) = yield $ intercalate " " $ [eid ^. entityId, encodeText (wid ^. wikiRef)]
formatMapping FormatSql = toFields .| toSqlDefault mappingsTable mappingFields where
  toFields = awaitForever convertRecord
  convertRecord (EntityMapping eid wid) = yield $ fieldsFormat eid wid
  mappingsTable = Table "WikiMappings"
  mappingFields = Fields ["entity", "wikiTitle"]
  fieldsFormat eid wid = FieldValues [eid ^. entityId, encodeText (wid ^. wikiRef)]