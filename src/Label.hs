{-# LANGUAGE OverloadedStrings #-}

module Label where

import Data.Text as T
import Data.String
import Control.Lens
import Conduit

import CoreTypes
import Format
import DataStream
import Sql

data Label = Label EntityId Text deriving Show

formatLabel :: Monad m => Format -> DataStream Label Text m
formatLabel FormatRDF = awaitForever convert where
  convert (Label eid value) = yield $ intercalate " " $ [eid ^. entityId, value]
formatLabel FormatSql = toFields .| toSqlDefault labelsTable labelFields where
  toFields = awaitForever convertRecord
  convertRecord (Label eid value) = yield $ fieldsFormat eid value
  labelsTable = Table "Labels"
  labelFields = Fields ["entity", "label"]
  fieldsFormat eid value = FieldValues [eid ^. entityId, value]