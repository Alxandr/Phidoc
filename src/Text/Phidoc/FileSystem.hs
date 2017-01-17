module Text.Phidoc.FileSystem
  ( MonadFS (..)
  , PathType (..)
  ) where

import Control.Monad.Catch
import System.FilePath
import System.Directory
import System.IO

data PathType = File | Directory | PathNotFound
  deriving Eq

class MonadThrow m => MonadFS m where
  readFile :: FilePath -> m String
  stat :: FilePath -> m PathType

instance MonadFS IO where
  readFile = Prelude.readFile
  stat path = doesDirectoryExist path >>= \dir ->
    if dir then return Directory
    else doesFileExist path >>= \file ->
      if file then return File
      else return PathNotFound
