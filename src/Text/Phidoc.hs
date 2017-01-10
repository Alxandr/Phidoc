module Text.Phidoc
  ( relink
  ) where

import Text.Pandoc (
    Inline (
        Link
      , Image
    )
  , Pandoc
  , readMarkdown
  , def
  )

import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Walk (query)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Text.Lazy as T

type Path = [T.Text]

{- | Joins a path into a string
-}
pathStr :: Path -> T.Text
pathStr = T.intercalate (T.pack "/")

extractUrl :: Inline -> [T.Text]
extractUrl (Link _ _ (u, _)) = [T.pack u]
extractUrl (Image _ _ (u, _)) = [T.pack u]
extractUrl _ = []

extractDocUrls :: Pandoc -> [T.Text]
extractDocUrls = query extractUrl

relPath :: Path -> T.Text -> Maybe T.Text
relPath [] _ = Nothing
relPath path url | T.head url == '.' = Just $ pathStr (path ++ [url])
relPath _ _ = Nothing

getPaths :: Path -> Pandoc -> [(T.Text, T.Text)]
getPaths path doc = paths where
  urls = extractDocUrls doc
  paths' = fmap (\url -> fmap (\path -> (url, path)) (relPath path url)) urls
  paths = catMaybes paths'

replacePaths :: T.Text -> [(T.Text, T.Text)] -> T.Text
replacePaths = foldl (\c (from, to) -> T.replace from to c)

relink :: [String] -> String -> Either PandocError String
relink [] content = Right content
relink path' content' = relinked where
  path = fmap T.pack path'
  content = T.pack content'
  doc = readMarkdown def content'
  pathFixes = fmap (getPaths path) doc
  relinked = fmap (T.unpack . replacePaths content) pathFixes