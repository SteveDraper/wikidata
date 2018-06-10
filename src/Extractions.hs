module Extractions where

import Data.Set
import Data.Foldable
import Data.Monoid
import Data.String

data Extraction = ExtractClaims | ExtractEntities | ExtractLabels deriving (Eq, Ord, Enum, Bounded, Show)

data ExtractionSet = ExtractionSet (Set Extraction) deriving (Show)

instance IsString ExtractionSet where
  fromString = extraction

instance Monoid ExtractionSet where
  mempty = ExtractionSet empty
  mappend (ExtractionSet s1) (ExtractionSet s2) = ExtractionSet $ s1 <> s2

allExtractions = ExtractionSet $ fromList [minBound..maxBound]

extraction :: String -> ExtractionSet
extraction s = ExtractionSet $ foldMap toExtraction s where
  toExtraction 'c' = singleton ExtractClaims
  toExtraction 'e' = singleton ExtractEntities
  toExtraction 'l' = singleton ExtractLabels
  toExtraction _ = mempty

containsExtraction :: Extraction -> ExtractionSet -> Bool
containsExtraction e (ExtractionSet es) = member e es