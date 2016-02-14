{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    , D.XdgDirectory (..), getXdgDirectory
    , withCurrentDirectory, getCurrentDirectory
    , doesDirectoryExist, getDirectoryFiles, getFilesRecursive
    , createDirectoryIfMissing, removeDirectoryRecursive
    -- * File actions
    , readFile, writeFile, copyFile, withFile
    , doesFileExist
    -- * Modification and conversion
    , stripDir, parentDirs, takeExtension
    , dirFromRel, fileFromRel
    , parseDir, parseFile, parseRelDir, parseRelFile
    -- * Environment
    , findExecutable
    ) where

import Prelude hiding (readFile, writeFile)

import Control.Exception (SomeException, bracket, displayException)
import Control.Monad (join, filterM, when)
import Control.Monad.Base (liftBase)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (mapMaybe)
import Path (
      Path, Rel, Abs, (</>), toFilePath
    , parseAbsDir, parseAbsFile)
import qualified Path
import qualified Path.Internal as PI
import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified System.IO as IO
import qualified System.IO.Temp as Tmp
#ifdef Unixy
import System.Posix.Files as U
#endif

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

-- | Perform an action with a file handle.
withFile :: (MonadBaseControl IO m) => File -> IO.IOMode -> (IO.Handle -> m a) -> m a
withFile f = liftBaseOp . IO.withFile (toFilePath f)

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

findExecutable :: (MonadIO m) => String -> m (Maybe File)
findExecutable = liftIO . fmap (parseAbsFile =<<) . D.findExecutable

-- | Obtain the paths to special directories for storing user-specific
-- application data, configuration, and cache files, conforming to the
-- XDG Base Directory Specification. See 'D.getXdgDirectory'.
getXdgDirectory :: (MonadIO m)
                => D.XdgDirectory
                -> RelDir
                -> Bool   -- ^ Create the directory if it does not exist
                -> m Dir
getXdgDirectory typ rel creat = do
    d <- liftIO $ parseAbsDir =<< D.getXdgDirectory typ (toFilePath rel)
    when creat $ do
        createDirectoryIfMissing True d
#ifdef Unixy
        liftIO $ U.setFileMode (toFilePath d) U.ownerModes
#endif
    pure d

-- | Try to parse an absolute directory path.
parseDir :: String -> Either String Dir
parseDir = parseThing parseAbsDir

-- | Try to parse an absolute file path.
parseFile :: String -> Either String File
parseFile = parseThing parseAbsFile

-- | Try to parse a relative directory path.
parseRelDir :: String -> Either String RelDir
parseRelDir = parseThing Path.parseRelDir

-- | Try to parse a relative file path.
parseRelFile :: String -> Either String RelFile
parseRelFile = parseThing Path.parseRelFile

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

getDirectoryFiles :: (MonadIO m) => Dir -> m [RelFile]
getDirectoryFiles dir = do
    fns <- filteredDirContents dir
    liftIO . chk doesFileExist $ mapMaybe Path.parseRelFile fns
  where
    chk f = filterM $ f . (dir </>)

getFilesRecursive :: (MonadIO m) => Dir -> m [RelFile]
getFilesRecursive dir = do
    fns <- filteredDirContents dir
    fs <- liftIO . chk doesFileExist $ mapMaybe Path.parseRelFile fns
    ds <- liftIO . chk doesDirectoryExist $ mapMaybe Path.parseRelDir fns
    (fs ++) . join <$> traverse (runDir dir) ds
  where
    runDir top d = fmap (d </>) <$> getFilesRecursive (top </> d)
    chk f = filterM $ f . (dir </>)

filteredDirContents :: (MonadIO m) => Dir -> m [FilePath]
filteredDirContents = fmap (filter (`notElem` [".", ".."])) . liftIO . D.getDirectoryContents . toFilePath

parentDirs :: Path Rel t -> [RelDir]
parentDirs = reverse . go
  where
    go :: Path Rel t -> [RelDir]
    go p = if str == "." then [] else pth : go pth
      where
        str = FP.takeDirectory . FP.dropTrailingPathSeparator $ toFilePath p
        pth = PI.Path $ normalizeDir str

-- | Get the extension of a file, returns @""@ for no extension, @".ext"@ otherwise.
takeExtension :: Path t File -> String
takeExtension = FP.takeExtension . toFilePath

-- | Internal use for normalizing a directory.
normalizeDir :: FilePath -> FilePath
normalizeDir =
  clean . FP.addTrailingPathSeparator . FP.normalise
  where clean "./" = ""
        clean ('/':'/':xs) = clean ('/':xs)
        clean x = x
