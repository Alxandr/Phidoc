module Text.Phidoc.Paths
  ( relink
  ) where

import           Text.Pandoc         (Inline (Image, Link), Pandoc, def,
                                      readMarkdown)

import           Control.Monad.Catch
import           Data.List           (intercalate)
import           Data.Maybe          (catMaybes)
import qualified Data.Text           as T
import           System.FilePath     (FilePath, normalise, (</>))
import           Text.Pandoc.Error   (PandocError)
import           Text.Pandoc.Walk    (query)

type CurrentPath = FilePath
type Replacement = (FilePath, FilePath)

fixPath :: String -> String
fixPath path = fixed
  where
    replacements =
      [ (T.pack "%20", T.pack " ") ]
    path'  = T.pack path
    fixed' = foldl (\p (from, to) -> T.replace from to p) path' replacements
    fixed  = T.unpack fixed'

extractLocal :: CurrentPath -> FilePath -> [Replacement]
extractLocal current path'@('.':_) = let path = fixPath path' in [(path, normalise (current </> path))]
extractLocal _ _                   = []

extractUrl :: CurrentPath -> Inline -> [Replacement]
extractUrl current (Link _ _ (u, _))  = extractLocal current u
extractUrl current (Image _ _ (u, _)) = extractLocal current u
extractUrl _ _                        = []

extractDocUrls :: CurrentPath -> Pandoc -> [Replacement]
extractDocUrls current = query (extractUrl current)

replacePaths :: String -> [Replacement] -> String
replacePaths content replacements = replaced where
  pack = T.pack
  unpack = T.unpack
  reducer :: T.Text -> Replacement -> T.Text
  reducer c (from, to) = T.replace (T.pack $ "](" ++ from ++ ")") (T.pack  $ "](" ++ to ++ ")") c
  content' = pack content
  replaced' = foldl reducer content' replacements
  replaced = unpack replaced'

relink :: MonadThrow m => CurrentPath -> String -> m String
relink current content = either throwM doReplace doc
  where
    doc = readMarkdown def content
    doReplace doc = return $ replacePaths content (extractDocUrls current doc)
