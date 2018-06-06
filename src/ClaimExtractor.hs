{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ClaimExtractor where

import Data.Text
import Data.Tuple
import Data.Maybe
import Data.Aeson
import Control.Lens
import Control.Lens.Traversal
import Data.Aeson.Lens
import Data.Map.Lens
import Conduit
import qualified Data.Conduit.Combinators as CC

import CoreTypes
import DataStream
import Wiki
import Claim
import Extract
import Routable

claimExtractor :: Monad m => Extractor m
claimExtractor = mapC fromWiki .|
                  filterJust .|
                  CC.concat .|
                  mapC RoutableClaim where
  fromWiki WikiRecord{..} = (extractClaims =<< ((,) <$> claimsObject <*> idValue)) where
    claimsObject = body ^? key "claims" . _Object
    idValue = fmap EntityId $ body ^? key "id" . _String


extractClaims :: (Object, EntityId) -> Maybe [Claim]
extractClaims (claimsObj, e_id) = claims where
  claims = ifoldMap makeClaims claimsObj
  makeClaims rel propertyObj = traverse makeClaim claimProperties where
    claimProperties = propertyObj ^.. values . key "mainsnak" . filtered (has (key "property"._String.only rel))
    makeClaim :: Value -> Maybe Claim
    makeClaim cp = fmap toClaim related where
      related = cp ^? key "datavalue" . key "value" . key "id" . _String
      toClaim target = Claim e_id (RelId rel) (EntityId target)


hasSubclass :: WikiRecord -> Bool
hasSubclass WikiRecord{..} = isJust $ body ^? key "claims" . key "P279"
