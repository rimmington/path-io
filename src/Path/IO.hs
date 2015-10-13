{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys <rimmington@gmail.com>
Stability   : provisional
Portability : portable

IO with well-typed paths.
-}

module Path.IO (
    -- * Types
      Dir, File, RelDir, RelFile
    -- * Directory actions
    , withSystemTempDirectory
    , withCurrentDirectory, getCurrentDirectory
    , createDirectoryIfMissing, removeDirectoryRecursive, doesDirectoryExist
    -- * File actions
    , readFile, writeFile, copyFile
    , doesFileExist
    -- * Modification and conversion
    , stripDir
    , dirFromRel, fileFromRel
    , parseDir, parseFile ) where

import Prelude hiding (readFile, writeFile)

import Control.Exception (SomeException, bracket, displayException)
import Control.Monad.Base (liftBase)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import Path (Path, Rel, Abs, toFilePath, parseAbsDir, parseAbsFile)
import qualified Path
import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified System.IO.Temp as Tmp

-- | An absolute path to a directory.
type Dir     = Path Abs Path.Dir
-- | An absolute path to a file.
type File    = Path Abs Path.File
-- | A relative path to a directory.
type RelDir  = Path Rel Path.Dir
-- | A relative path to a file.
type RelFile = Path Rel Path.File

-- | Perform some action with a temporary directory.
withSystemTempDirectory :: (MonadBaseControl IO m) => String -> (Dir -> m r) -> m r
withSystemTempDirectory pf act = liftBaseOp (Tmp.withSystemTempDirectory pf) $ \path -> do
    dir <- liftBase $ parseAbsDir path
    act dir

-- | Perform some action with a different working directory, then set it back.
withCurrentDirectory :: (MonadBaseControl IO m) => Dir -> m a -> m a
withCurrentDirectory dir act =
    liftBaseOp (bracket D.getCurrentDirectory D.setCurrentDirectory) . const $ liftBase (D.setCurrentDirectory $ toFilePath dir) *> act

-- | Get the current working directory.
getCurrentDirectory :: (MonadIO m) => m Dir
getCurrentDirectory = liftIO $ parseAbsDir =<< D.getCurrentDirectory

-- | Lazily read from a file.
readFile :: (MonadIO m) => File -> m BL.ByteString
readFile = liftIO . BL.readFile . toFilePath

-- | Write to a file.
writeFile :: (MonadIO m) => File -> BL.ByteString -> m ()
writeFile fp = liftIO . BL.writeFile (toFilePath fp)

-- | Create a directory (and optionally any parents) if required.
createDirectoryIfMissing :: (MonadIO m) => Bool -> Dir -> m ()
createDirectoryIfMissing createParents = liftIO . D.createDirectoryIfMissing createParents . toFilePath

-- | Removes a directory and all contents.
-- Be careful, if the directory contains symlinks, the function will follow them.
removeDirectoryRecursive :: (MonadIO m) => Dir -> m ()
removeDirectoryRecursive = liftIO . D.removeDirectoryRecursive . toFilePath

-- | Is a directory at the specified path?
doesDirectoryExist :: (MonadIO m) => Dir -> m Bool
doesDirectoryExist = liftIO . D.doesDirectoryExist . toFilePath

-- | Is a file at the specified path?
doesFileExist :: (MonadIO m) => File -> m Bool
doesFileExist = liftIO . D.doesFileExist . toFilePath

-- | If the new file already exists, it is atomically replaced by the old file.
-- The permissions of old are copied to new, if possible.
copyFile :: (MonadIO m) => File -> File -> m ()
copyFile a = liftIO . D.copyFile (toFilePath a) . toFilePath

-- | Try to parse an absolute directory path.
parseDir :: String -> Either String Dir
parseDir = parseThing parseAbsDir

-- | Try to parse an absolute file path.
parseFile :: String -> Either String File
parseFile = parseThing parseAbsFile

parseThing :: (String -> Either SomeException a) -> String -> Either String a
parseThing f = first displayException . f

-- | Parse a directory path, prefixing the provided 'Dir' if it looks relative.
dirFromRel :: Dir -> String -> Either String Dir
dirFromRel = pathFromRel parseDir

-- | Parse a file path, prefixing the provided 'Dir' if it looks relative.
fileFromRel :: Dir -> String -> Either String File
fileFromRel = pathFromRel parseFile

-- | Strip a leading directory prefix. Fails if the prefix doesn't actually
-- prefix the provided path. See also 'Path.stripDir'.
stripDir :: Path b Path.Dir -> Path b t -> Either String (Path Rel t)
stripDir parent = first displayException . Path.stripDir parent

pathFromRel :: (String -> m a) -> Dir -> String -> m a
pathFromRel f cwd = f . FP.normalise . toAbsApprox
  where
    toAbsApprox s | FP.isRelative s = toFilePath cwd FP.</> s
                  | otherwise       = s
