{-# LANGUAGE OverloadedStrings #-}

module Claim where

import Data.Text as T
import Data.Set
import Data.String
import Control.Lens
import Conduit

import CoreTypes
import Filter
import Format
import DataStream
import Sql

data Claim = Claim EntityId RelId EntityId deriving Show

data ClaimFilter = ClaimFilter (Set RelId) deriving Show

instance IsString ClaimFilter where
  fromString = makeClaimFilter . pack

formatClaim :: Monad m => Format -> DataStream Claim Text m
formatClaim FormatRDF = awaitForever convert where
  convert (Claim eid relid rid) = yield $ rdfFormat eid relid rid
  rdfFormat eid relid rid = intercalate " " [eid ^. entityId, relid ^. relId, rid ^. entityId]
formatClaim FormatSql = toFields .| toSqlDefault claimsTable claimFields where
  toFields = awaitForever convertRecord
  convertRecord (Claim eid relid rid) = yield $ fieldsFormat eid relid rid
  claimsTable = Table "Claims"
  claimFields = Fields ["entity", "relation", "target"]
  fieldsFormat eid relid rid = FieldValues [eid ^. entityId, relid ^. relId, rid ^. entityId]

makeClaimFilter :: Text -> ClaimFilter
makeClaimFilter t = ClaimFilter $ fromList $ fmap RelId $ T.words t

applyClaimFilter :: ClaimFilter -> Claim -> Bool
applyClaimFilter (ClaimFilter rs) (Claim _ r _) = member r rs
