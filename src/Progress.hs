{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Progress where

import Control.Monad.State
import Katip
import Data.Text
import Data.Semigroup

import DataStream

data DSState = DSState {
  processed :: Int,
  resolved :: Int
} deriving Show

initialState :: DSState
initialState = DSState 0 0

updateRecordsProcessed :: MonadState DSState m => DataStream a a m
updateRecordsProcessed = withEffectC $ (modify incCount) where
  incCount DSState{..} = DSState (processed + 1) resolved

updateRecordsResolved:: MonadState DSState m => DataStream a a m
updateRecordsResolved = withEffectC $ (modify incCount) where
  incCount DSState{..} = DSState processed (resolved + 1)

logProgress :: (KatipContext m, MonadState DSState m) => Int -> DataStream a a m
logProgress period = withEffectC $ logEvery period where
  logEvery n = do
    state <- get
    case state of
      DSState{..} ->
        if processed `mod` period == 0 && processed > 0 then
          logFM InfoS $ logStr $ "Processed: " <> (show processed) <> ", Resolved: " <> (show resolved)
        else
          return ()
