{-# LANGUAGE OverloadedStrings #-}

module Sql where

import DataStream
import Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Text
import Data.Semigroup
import Control.Monad.State.Lazy

newtype Table = Table Text
newtype FieldValues = FieldValues [Text]
newtype Fields = Fields [Text]

data EmissionState = EmissionState Int

class SqlFormat a where
  render :: a -> Text

instance SqlFormat Table where
  render (Table name) = "`" <> name <> "`"

instance SqlFormat FieldValues where
  render (FieldValues fv) = sqlValueList quote fv

instance SqlFormat Fields where
  render (Fields fs) = sqlValueList id fs

toSql :: Monad m => Int -> Table -> Fields -> DataStream FieldValues Text m
toSql period t f = withLocalState emit close (EmissionState 0) .| CC.concat where
  emit :: FieldValues -> State EmissionState [Text]
  emit fv = do
    s <- get
    inc
    case s of
      EmissionState n | (n `mod` period) == 0 -> return $ emitInsert <> emitValues False fv
      EmissionState n | (n `mod` period) == (period - 1) -> return $ emitValues True fv <> eos
      EmissionState n -> return $ emitValues True fv
  close = do
    s <- get
    case s of
      EmissionState n | (n `mod` period) == 0 -> return []
      _ -> return eos
  inc :: State EmissionState ()
  inc = modify (\(EmissionState n) -> EmissionState $ n + 1)
  eos = [";"]
  emitInsert = [intercalate " " ["INSERT IGNORE INTO", render t, render f, "VALUES"]]
  emitValues True fv = ["," <> render fv]
  emitValues False fv = [render fv]

toSqlDefault :: Monad m => Table -> Fields -> DataStream FieldValues Text m
toSqlDefault = toSql 1000

sqlValueList :: (Text -> Text) -> [Text] -> Text
sqlValueList escaping l = "(" <> (intercalate "," (fmap escaping l)) <> ")" where

quote :: Text -> Text
quote t = "'" <> escape t <> "'" where
  escape = (replace "'" "''") . replace "\\" "\\\\"