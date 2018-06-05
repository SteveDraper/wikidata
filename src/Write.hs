{-# LANGUAGE OverloadedStrings #-}

module Write where

import Data.Text
import Data.Foldable
import Data.ByteString
import System.IO(Handle, stdout)
import Data.Conduit.Binary
import Data.Conduit.Text
import Conduit

import Wiki
import Extract
import DataStream
import FileSpec

type Writer m = DataStream ByteString Void m

simpleWriter :: (MonadResource m, MonadThrow m) => OutputFile -> Writer m
simpleWriter StdOut = sinkHandle stdout
simpleWriter (OutputFile f) = sinkFile f
