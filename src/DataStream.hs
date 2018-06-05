{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module DataStream where

import Data.ByteString
import Data.Maybe
import Conduit
import Data.Conduit
import Data.Conduit.Binary as CB
import Data.Conduit.List as Cl
import Data.Conduit.Text
import qualified Data.Conduit.Text as Ct
import Data.Aeson
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.Functor
import Control.Monad.State.Lazy

import FileSpec
import System.IO
import qualified Data.Text as T
import Data.Text.IO as DTIO

type DataStream i o m = ConduitT i o m ()

filterJust :: Monad m => DataStream (Maybe a) a m
filterJust = do
   maybeVal <- await
   case maybeVal of
     Nothing -> return ()
     Just Nothing -> filterJust
     Just (Just a) -> (yield a) >> filterJust

intercalateC :: Monad m => a -> DataStream a a m
intercalateC a = awaitForever addSuffix where
  addSuffix l = yield l >> yield a

linesToBytes :: MonadThrow m => DataStream T.Text ByteString m
linesToBytes = addLinefeed .| Ct.encode Ct.utf8 where
  addLinefeed = intercalateC "\n"

withEffectC :: Monad m => m b -> DataStream a a m
withEffectC e = awaitForever $ withEffect e where
  withEffect e a = lift (void e) >> yield a

withLocalState :: Monad m => (a -> State s b) -> State s b -> s -> DataStream a b m
withLocalState trans close = fromState where
  fromState i = do
    maybe_a <- await
    case maybe_a of
      Nothing -> do
        let (b, _) = runState close i
        yield b
        return ()
      Just a -> do
        let (b, s) = runState (trans a) i
        yield b
        fromState s

