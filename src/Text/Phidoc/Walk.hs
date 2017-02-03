module Text.Phidoc.Walk
  ( walk
  , FileContent (..)
  ) where

import           Control.Monad.Catch
import           Data.Typeable          (Typeable)
import           Prelude                hiding (readFile)
import           System.FilePath
import           Text.Phidoc.FileSystem
import           Text.Phidoc.Resolve

data FileContent = Content FilePath String
                 | Meta FilePath String
  deriving (Eq, Show)

data WalkException =
    ExtensionNotSupported String
  deriving Typeable
instance Show WalkException where
  show (ExtensionNotSupported ext) = "Extension '" ++ ext ++ "' is not supported."
instance Exception WalkException

validImport :: String -> Bool
validImport []      = False
validImport ('#':_) = False
validImport _       = True

process :: MonadFS m => FilePath -> m [FileContent]
process file | takeExtension file == ".md"   = processMD file
process file | takeExtension file == ".phi"  = processPHI file
process file | takeExtension file == ".yml"  = processYml file
process file | takeExtension file == ".yaml" = processYml file
process file = throwM $ ExtensionNotSupported $ takeExtension file

processYml :: MonadFS m => FilePath -> m [FileContent]
processYml path = do
  content <- readFile path
  return [Meta path content]

processMD :: MonadFS m => FilePath -> m [FileContent]
processMD path = do
  content <- readFile path
  return [Content path content]

processPHI :: MonadFS m => FilePath -> m [FileContent]
processPHI path = do
  content <- readFile path
  let contentLines = lines content
  let imports = filter validImport contentLines
  let dir = takeDirectory path
  contents <- mapM (walk . normalise . (dir </>)) imports
  return $ concat contents

walk :: MonadFS m => FilePath -> m [FileContent]
walk path = do
  (ResolvedDoc doc) <- resolve path
  process doc
