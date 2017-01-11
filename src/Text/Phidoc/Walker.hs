module Text.Phidoc.Walker
  ( walk
  ) where

import Text.Phidoc.Paths (relink)
import System.FilePath
import System.Directory
import System.IO
import Data.List (intercalate)
import qualified Data.Text as T

findIndex :: FilePath -> IO (Maybe FilePath)
findIndex dir = do
  let phiPath = dir </> "index.phi"
  let mdPath  = dir </> "index.md"
  phiExists <- doesFileExist phiPath
  if phiExists
  then return (Just phiPath)
  else do
    mdExists <- doesFileExist mdPath
    if mdExists
    then return (Just mdPath)
    else return Nothing

assertNewLineEnding :: String -> String
assertNewLineEnding = T.unpack . flip T.snoc '\n' . T.stripEnd . T.pack

processMd :: FilePath -> FilePath -> IO String
processMd rel file = do
  content <- readFile file
  let dir = takeDirectory file
  let result = relink dir content
  either (fail . show) return result

processPhi rel file = do
  phiContent <- readFile file
  let phiLines = lines phiContent
  let dir = normalise $ rel </> takeDirectory file
  fileContents <- mapM (walkFile dir . normalise) phiLines
  let withNewLines = fmap assertNewLineEnding fileContents
  return $ intercalate "\n" withNewLines

process :: FilePath -> FilePath -> IO String
process rel file | takeExtension file == ".md"  = processMd rel file
process rel file | takeExtension file == ".phi" = processPhi rel file
process _   file                                = fail $ "Extension " ++ takeExtension file ++ " is not supported by phidoc."

walkFile :: FilePath -> FilePath -> IO String
walkFile rel file'' = do
  let file' = rel </> file''
  isDir  <- doesDirectoryExist file'
  file   <- if isDir
            then findIndex file'
            else return (Just file')

  case file of
    Nothing -> fail (file' ++ " is a directory with no valid index file inside.")
    Just f  -> do
      isFile <- doesFileExist f
      if isFile
      then process rel f
      else fail ("'" ++ f ++ "' does not exist.")

walk :: FilePath -> IO String
walk = walkFile ""
