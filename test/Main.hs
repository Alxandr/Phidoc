module Main where

import           Control.Monad
import           Control.Monad.Catch
import           FakeFileSystem
import           Prelude                hiding (readFile)
import           Test.HUnit
import           Text.Phidoc.FileSystem (readFile)

import qualified Text.Phidoc.Paths      as P
import qualified Text.Phidoc.Resolve    as R
import qualified Text.Phidoc.Walk       as W

fs :: FakeFileSystem
fs = "index.phi"            |= "# Meta\n\
                               \./meta\n\
                               \\n\
                               \# Chapters\n\
                               \./ch 1\n\
                               \./ch 2\n\
                               \./ch 3\n"

  /+ "meta.yml"             |= "title: Some Title\n"

  /+ "ch 1" </> "index.md"  |= "# Chapter 1\n\
                               \This is chapter 1\n"

  /+ "ch 2" </> "index.md"  |= "# Chapter 2\n\
                               \This is chapter 2\n"

  /+ "ch 3" </> "index.phi" |= "./intro\n\
                               \./part1\n\
                               \./part2\n"

  /+ "ch 3" </> "intro.md"  |= "# Chapter 3\n\
                               \This is chapter\n"

  /+ "ch 3" </> "part1.md"  |= "## Part 1\n\
                               \This is part 1\n"

  /+ "ch 3" </> "part2.md"  |= "## Part 2\n\
                               \This is part 2\n"
  /+ empty

expect :: (Eq a, Show a) => a -> FakeIO a -> Assertion
expect expected mactual = assertion
  where
    result    = fakeRun mactual fs
    assertion = either throwM (assertEqual "" expected . fst) result

fsRead :: (FilePath -> String -> W.FileContent) -> FilePath -> W.FileContent
-- fsRead path = either (\_ -> error "file not found in fakefs: " ++ path) (W.FileContent path . fst) $ flip fakeRun fs $ readFile path
fsRead fn path = fileContent
  where
    fakeIo = flip fakeRun fs $ readFile path
    content = either (\_ -> error $ "file not found in fakefs: " ++ path) fst fakeIo
    fileContent = fn path content

fsReadContent :: FilePath -> W.FileContent
fsReadContent = fsRead W.Content

fsReadMeta :: FilePath -> W.FileContent
fsReadMeta = fsRead W.Meta

resolveTests :: Test
resolveTests = test [
    "resolve empty to index.phi"     ~: expect (R.ResolvedDoc "index.phi") $ R.resolve ""
  , "resolve index to index.phi" ~: expect (R.ResolvedDoc "index.phi") $ R.resolve "index"
  , "resolve ch 1 to index.md"   ~: expect (R.ResolvedDoc $ "ch 1" </> "index.md") $ R.resolve "ch 1"
  , "resolve ch 2 to index.md"   ~: expect (R.ResolvedDoc $ "ch 2" </> "index.md") $ R.resolve "ch 2"
  , "resolve ch 1/index to index.md" ~: expect (R.ResolvedDoc $ "ch 1" </> "index.md") $ R.resolve $ "ch 1" </> "index"
  ]

walkTests :: Test
walkTests = test [
    "walk should return a markdown file as is" ~: expect [fsReadContent $ "ch 1" </> "index.md"] $ W.walk "ch 1"
  , "walk should return all markdown files pointed at from a phi file" ~: expect [
      fsReadContent $ "ch 3" </> "intro.md"
    , fsReadContent $ "ch 3" </> "part1.md"
    , fsReadContent $ "ch 3" </> "part2.md"
    ] $ W.walk "ch 3"
  , "walk should run recursively" ~: expect [
      fsReadMeta "meta.yml"
    , fsReadContent $ "ch 1" </> "index.md"
    , fsReadContent $ "ch 2" </> "index.md"
    , fsReadContent $ "ch 3" </> "intro.md"
    , fsReadContent $ "ch 3" </> "part1.md"
    , fsReadContent $ "ch 3" </> "part2.md"
    ] $ W.walk ""
  ]

rewriteTests :: Test
rewriteTests = test [
    "rewrite should replace links starting with a ." ~: expect ("[foo](bar" </> "foo)") $ P.relink "bar" "[foo](./foo)"
  , "rewrite should replace image hrefs starting with a ."  ~: expect ("![foo](bar" </> "foo)") $ P.relink "bar" "![foo](./foo)"
  ]

tests :: Test
tests = test [
    "resolve tests" ~: resolveTests
  , "walk tests"    ~: walkTests
  , "rewrite tests" ~: rewriteTests
  ]

runTests :: Test -> IO ()
runTests tests = do
  -- print fs
  runTestTT tests
  return ()

main :: IO ()
main = runTests tests
