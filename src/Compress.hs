module Compress where

import DataStream
import Data.Conduit.Zlib
import Data.ByteString
import Conduit

data Compress = Compression | NoCompression deriving Show

maybeCompress :: (PrimMonad m, MonadThrow m) => Compress -> DataStream a ByteString m -> DataStream a ByteString m
maybeCompress Compression w = w .| gzip
maybeCompress NoCompression w = w