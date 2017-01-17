module Text.Phidoc.Resolve
  ( resolve
  , ResolvedDoc (..)
  ) where

import Data.Typeable (Typeable)
import Control.Monad.Catch
import System.FilePath
import Text.Phidoc.FileSystem

data ResolvedDoc = ResolvedDoc FilePath

data ResolveException =
    PathDoesNotExist String
  deriving Typeable
instance Show ResolveException where
  show (PathDoesNotExist path) = "Path '" ++ path ++ "' does not exist."
instance Exception ResolveException

instance Show ResolvedDoc where
  show (ResolvedDoc path) = "ResolvedDoc(path=" ++ path ++ ")"

resolve' :: MonadFS m => Bool -> FilePath -> m ResolvedDoc
resolve' allowDir path = do
  pathType <- stat path
  case (pathType, allowDir) of
    (File, _)         -> return $ ResolvedDoc path
    (Directory, True) -> resolve' False $ path </> "index"
    _                 -> do
      let mdPath = path ++ ".md"
      mdType <- stat mdPath
      if mdType == File then return $ ResolvedDoc mdPath
      else do
        let phiPath = path ++ ".phi"
        phiType <- stat phiPath
        if phiType == File then return $ ResolvedDoc phiPath
        else throwM $ PathDoesNotExist path

resolve :: MonadFS m => FilePath -> m ResolvedDoc
resolve = resolve' True
