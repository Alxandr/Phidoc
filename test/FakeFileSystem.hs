{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
module FakeFileSystem (
    FakeFileSystem
  , FakeIO (..)
  , empty
  , (</>)
  , (/+)
  , (|=)
  ) where

import           Control.Monad.Catch
import           Data.HashMap           hiding (empty)
import           Data.Maybe
import           Data.Monoid
import           Data.Typeable          (Typeable)
import           Prelude                hiding (lookup)
import           System.FilePath
import           Test.HUnit.Base
import           Text.Phidoc.FileSystem

import qualified Data.HashMap           as M
import qualified Data.Text              as T

data FakeFileSystem = FakeFS FakeEntry
instance Show FakeFileSystem where
  show (FakeFS fs) = show fs

data FakeEntry =
    FakeFile String
  | FakeDir (Map String FakeEntry)
instance Show FakeEntry where
  show (FakeFile content) = "<File>"
  show (FakeDir entries) = T.unpack text
    where
      text = T.concat [T.pack "[", mapped, T.pack "]"]
      mapped = foldWithKey buildStr T.empty entries
      buildStr name entry = buildText (T.pack name) (T.pack $ show entry)
      buildText name entry txt = T.concat [
          txt
        , T.pack "\n"
        , name
        , T.pack ": "
        , T.replace (T.pack "\n") (T.pack "\n  ") entry
        ]

empty :: FakeFileSystem
empty = FakeFS (FakeDir M.empty)

infixr 1 /+
(/+) :: (FilePath, String) -> FakeFileSystem -> FakeFileSystem
(/+) (p, c) = addEntry p c

infix 2 |=
(|=) :: FilePath -> String -> (FilePath, String)
(|=) = (,)

addFile :: String -> Maybe FakeEntry -> Maybe FakeEntry
addFile _       (Just _) = error "Entry already exist"
addFile content Nothing  = Just $ FakeFile content

mergeDir :: [FilePath] -> String -> Maybe FakeEntry -> Maybe FakeEntry
mergeDir path content (Just e) = Just $ insertEntry path content e
mergeDir path content Nothing  = Just $ insertEntry path content (FakeDir M.empty)

insertEntry :: [FilePath] -> String -> FakeEntry -> FakeEntry
insertEntry _           _       (FakeFile _)     = error "Cannot merge with file"
insertEntry [name]      content (FakeDir m) = FakeDir $ alter (addFile content) name m
insertEntry (name:path) content (FakeDir m) = FakeDir $ alter (mergeDir path content) name m

addEntry :: FilePath -> String -> FakeFileSystem -> FakeFileSystem
addEntry path content (FakeFS rootEntry) = newFS
  where
    path'  = splitDirectories path
    merged = insertEntry path' content rootEntry
    newFS  = FakeFS merged

lookupEntry :: FilePath -> FakeFileSystem -> Maybe FakeEntry
lookupEntry path (FakeFS rootEntry) = lookup' (splitDirectories path) rootEntry
  where
    lookup' :: [FilePath] -> FakeEntry -> Maybe FakeEntry
    lookup' []           e           = Just e
    lookup' (name:path) (FakeFile _) = Nothing
    lookup' (name:path) (FakeDir m)  = lookup name m >>= lookup' path

newtype FakeIO a = FakeIO { fakeRun :: FakeFileSystem -> Either SomeException (a, FakeFileSystem) }

fakeMap :: (a -> b) -> FakeIO a -> FakeIO b
fakeMap f ma = do
  a <- ma
  return $ f a

fakePure :: a -> FakeIO a
fakePure a = FakeIO (\fs -> Right (a, fs))

fakeAp :: FakeIO (a -> b) -> FakeIO a -> FakeIO b
fakeAp mf ma = do
  f <- mf
  a <- ma
  return $ f a

fakeBind :: (a -> FakeIO b) -> FakeIO a -> FakeIO b
fakeBind f ma = FakeIO bindFn
  where
    bindFn fs = fakeRun ma fs >>= mapFn
    mapFn (a, fs) = fakeRun (f a) fs

fakeThrow :: Exception e => e -> FakeIO a
fakeThrow = FakeIO . const . Left . toException

fakeRead :: FilePath -> FakeIO String
fakeRead path = FakeIO readEntry
  where
    readEntry fs = (, fs) <$> readEntry' fs
    readEntry' = fromMaybe (Left $ toException $ NotFound path) . file
    file = fmap fileContent . entry
    entry = lookupEntry path
    fileContent (FakeFile content) = Right content
    fileContent (FakeDir _)        = Left $ toException $ PathIsDir path

fakeStat :: FilePath -> FakeIO PathType
fakeStat path = FakeIO statEntry
  where
    statEntry fs = (, fs) <$> statEntry' fs
    statEntry' = fromMaybe (Right PathNotFound) . stat
    stat = fmap statType . entry
    entry = lookupEntry path
    statType (FakeFile _) = Right File
    statType (FakeDir _)  = Right Directory

instance Functor FakeIO where
  fmap = fakeMap

instance Applicative FakeIO where
  pure = fakePure
  (<*>) = fakeAp

instance Monad FakeIO where
  (>>=) = flip fakeBind

instance MonadThrow FakeIO where
  throwM = fakeThrow

instance MonadFS FakeIO where
  readFile = fakeRead
  stat     = fakeStat

data ReadException =
    PathIsDir String
  | NotFound String
  deriving (Show, Typeable)
instance Exception ReadException
