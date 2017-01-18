module Main where

import FakeFileSystem
import Control.Monad
import Control.Monad.Catch
import Test.HUnit

import qualified Text.Phidoc.Resolve as R

fs :: FakeFileSystem
fs = "index.phi"           |= "./abstract\n./ch 1\n./ch 2\n"
  /+ "abstract.md"         |= "This is the abstract"
  /+ "ch 1" </> "index.md" |= "# Chapter 1\n This is chapter 1"
  /+ "ch 2" </> "index.md" |= "# Chapter 2\n This is chapter 2"
  /+ empty

expect :: (Eq a, Show a) => a -> FakeIO a -> Assertion
expect expected mactual = assertion
  where
    result    = fakeRun mactual fs
    assertion = either throwM (assertEqual "" expected . fst) result

tests :: Test
tests = test [
    "resolve index to index.phi" ~: expect (R.ResolvedDoc "index.phi") $ R.resolve "index"
  , "resolve ch 1 to index.md"   ~: expect (R.ResolvedDoc $ "ch 1" </> "index.md") $ R.resolve "ch 1"
  , "resolve ch 2 to index.md"   ~: expect (R.ResolvedDoc $ "ch 2" </> "index.md") $ R.resolve "ch 2"
  , "resolve ch 1/index to index.md" ~: expect (R.ResolvedDoc $ "ch 1" </> "index.md") $ R.resolve $ "ch 1" </> "index"
  ]

runTests :: Test -> IO ()
runTests tests = do
  -- print fs
  runTestTT tests
  return ()

main :: IO ()
main = runTests tests
