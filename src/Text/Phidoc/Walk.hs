module Text.Phidoc.Walk
  ( walk
  , FileContent (..)
  ) where

import Prelude hiding (readFile)
import Data.Typeable (Typeable)
import Control.Monad.Catch
import Text.Phidoc.FileSystem
import Text.Phidoc.Resolve
import System.FilePath

data FileContent = FileContent FilePath String deriving (Eq, Show)

data WalkException =
    ExtensionNotSupported String
  deriving Typeable
instance Show WalkException where
  show (ExtensionNotSupported ext) = "Extension '" ++ ext ++ "' is not supported."
instance Exception WalkException

process :: MonadFS m => FilePath -> m [FileContent]
process file | takeExtension file == ".md"  = processMD file
process file | takeExtension file == ".phi" = processPHI file
process file                                = throwM $ ExtensionNotSupported $ takeExtension file

processMD :: MonadFS m => FilePath -> m [FileContent]
processMD path = do
  content <- readFile path
  return [FileContent path content]

processPHI :: MonadFS m => FilePath -> m [FileContent]
processPHI path = do
  content <- readFile path
  let imports = lines content
  let dir = takeDirectory path
  contents <- mapM (walk . normalise . (dir </>)) imports
  return $ concat contents

walk :: MonadFS m => FilePath -> m [FileContent]
walk path = do
  (ResolvedDoc doc) <- resolve path
  process doc
