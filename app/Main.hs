module Main where

import Text.Phidoc

toIO :: Show a => Either a b -> IO b
toIO (Left e) = fail (show e)
toIO (Right r) = return r

relinkIO :: [String] -> String -> IO String
relinkIO path content = toIO (relink path content)

someFunc :: IO ()
someFunc = do
  out <- relinkIO [] "Hello from someFun"
  putStrLn out

main :: IO ()
main = someFunc
