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
  ]

runTests :: Test -> IO ()
runTests = void . runTestTT

main :: IO ()
main = runTests tests
