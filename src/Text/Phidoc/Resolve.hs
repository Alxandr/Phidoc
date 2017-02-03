module Text.Phidoc.Resolve
  ( resolve
  , ResolvedDoc (..)
  ) where

import           Control.Monad.Catch
import           Data.Typeable          (Typeable)
import           System.FilePath
import           Text.Phidoc.FileSystem

data ResolvedDoc = ResolvedDoc FilePath deriving Eq

data ResolveException =
    PathDoesNotExist String
  deriving Typeable
instance Show ResolveException where
  show (PathDoesNotExist path) = "Path '" ++ path ++ "' does not exist."
instance Exception ResolveException

instance Show ResolvedDoc where
  show (ResolvedDoc path) = "ResolvedDoc(path=" ++ path ++ ")"

resolveWithExts :: MonadFS m => [String] -> FilePath -> m ResolvedDoc
resolveWithExts [] path = throwM $ PathDoesNotExist path
resolveWithExts (ext:rest) path = do
  let pathWithExt = path ++ ext
  fileType <- stat pathWithExt
  if fileType == File then return $ ResolvedDoc pathWithExt
  else resolveWithExts rest path

resolve' :: MonadFS m => Bool -> FilePath -> m ResolvedDoc
resolve' allowDir path = do
  pathType <- stat path
  case (pathType, allowDir) of
    (File, _)         -> return $ ResolvedDoc path
    (Directory, True) -> resolve' False $ path </> "index"
    _                 -> resolveWithExts [".phi", ".md", ".yml"] path

resolve :: MonadFS m => FilePath -> m ResolvedDoc
resolve = resolve' True
