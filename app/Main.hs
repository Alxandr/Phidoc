{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP    #-}
module Main where

import           Options.Applicative
import           Options.Applicative.Arrows
import           System.FilePath
import           Text.Phidoc

import qualified Data.Text                  as T

data Opts = Opts
  { file    :: String }
  deriving (Show)

version :: Parser (a -> a)
version = infoOption "0.1.0"
  ( long "version"
    <> help "Print version information" )

type Args = Opts
argsP :: Parser Args
argsP = runA $ proc () -> do
  opts <- asA optsP -< ()
  A version >>> A helper -< opts

optsP :: Parser Opts
optsP = Opts
  <$> argument str
    (  metavar "FILE"
    <> help "Input file" )

pinfo :: ParserInfo Args
pinfo = info argsP
  (  fullDesc
  <> progDesc "Create combined pandocs"
  <> header "this is currently just a test" )

assertNewLineEnding :: String -> String
assertNewLineEnding = T.unpack . flip T.snoc '\n' . flip T.snoc '\n' . T.stripEnd . T.pack

relinkContent :: FileContent -> IO String
relinkContent (FileContent path content) = do
  relinked <- relink (takeDirectory path) content
  return $ assertNewLineEnding relinked

main :: IO ()
main = do
  args   <- execParser pinfo
  result <- walk (file args)
  relinked <- mapM relinkContent result
  putStrLn $ concat relinked
