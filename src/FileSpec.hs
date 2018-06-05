module FileSpec where

import Data.String
import System.FilePath.Posix

data InputFile = StdIn | InputFile FilePath deriving Show
data OutputFile = StdOut | OutputFile FilePath deriving Show

instance IsString InputFile where
  fromString s = InputFile $ s

instance IsString OutputFile where
  fromString s = OutputFile $ s