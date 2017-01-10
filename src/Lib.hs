module Lib
    ( someFunc
    ) where

import Text.Phidoc

toIO :: Show a => Either a b -> IO b
toIO = either (fail . show) return

someFunc :: IO ()
someFunc = do
  out <- toIO $ relink [] "Hello from someFun"
  putStrLn out
