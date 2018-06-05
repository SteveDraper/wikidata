module Routable where

import Control.Lens

import Claim
import EntityMapping
import Filter
import Extractions

data Routable = RoutableClaim Claim |
                RoutableEntityMapping EntityMapping

toClaim :: Prism' Routable Claim
toClaim = prism' RoutableClaim getClaim where
  getClaim (RoutableClaim c) = Just c
  getClaim _ = Nothing

toEntityMapping :: Prism' Routable EntityMapping
toEntityMapping = prism' RoutableEntityMapping getClaim where
  getClaim (RoutableEntityMapping m) = Just m
  getClaim _ = Nothing

isClaim :: AllFilter Routable
isClaim = AllFilter $ has toClaim

isEntityMapping :: AllFilter Routable
isEntityMapping = AllFilter $ has toEntityMapping

isRequired :: ExtractionSet -> AllFilter Routable
isRequired s = AllFilter includedType where
  includedType :: Routable -> Bool
  includedType (RoutableClaim _) = containsExtraction ExtractClaims s
  includedType (RoutableEntityMapping _) = containsExtraction ExtractEntities s

toAllFilter :: ClaimFilter -> AllFilter Routable
toAllFilter cf = AllFilter ofRelation where
  ofRelation :: Routable -> Bool
  ofRelation (RoutableClaim c) = applyClaimFilter cf $ c
  ofRelation _ = True