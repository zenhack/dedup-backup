{-
    Copyright 2015 Ian Denhardt <ian@zenhack.net>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>
-}
module Main where

import qualified System.Posix.Files as PF
import qualified System.Posix.Types as PT
import System.Directory (getDirectoryContents, createDirectoryIfMissing, doesFileExist)
import Control.Monad (liftM, mapM_, unless)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base16 as Hex
import Data.ByteString.Char8 (unpack)
import System.Environment
import Data.List (stripPrefix)


-- This stuff exists for the testsuite's benift. When running the program, the
-- only instance of FileStatus we ever use is PF.FileStatus, but we can't
-- construct those, so we have our own type defined in the test suite for
-- testing:

class FileStatus a where
    isRegularFile    :: a -> Bool
    isDirectory      :: a -> Bool
    isSymbolicLink   :: a -> Bool
    fileMode         :: a -> PT.FileMode
    fileOwner        :: a -> PT.UserID
    fileGroup        :: a -> PT.GroupID
    accessTime       :: a -> PT.EpochTime
    modificationTime :: a -> PT.EpochTime

instance FileStatus PF.FileStatus where
    isRegularFile    = PF.isRegularFile
    isDirectory      = PF.isDirectory
    isSymbolicLink   = PF.isSymbolicLink
    fileMode         = PF.fileMode
    fileOwner        = PF.fileOwner
    fileGroup        = PF.fileGroup
    accessTime       = PF.accessTime
    modificationTime = PF.modificationTime


-- // from System.FilePath *almost* does what we want, but it drops left if
-- right starts with a slash.
(//) left right = left ++ "/" ++ right

data JobSpec = JobSpec { src   :: FilePath
                       , dest  :: FilePath
                       , blobs :: FilePath
                       , prev  :: Maybe FilePath
                       }

data FileTree s = Directory FilePath s [FileTree s]
                | RegularFile FilePath s
                | Symlink FilePath s
                | Unsupported FilePath s

data Action s = MkDir FilePath s [Action s]
              | MkSymlink FilePath s
              | DedupCopy FilePath s
              | Report String

-- | @(pathMap f tree)@ applies @f@ to all @FilePath@s in @tree@, recursively.
pathMap :: (FilePath -> FilePath) -> FileTree s -> FileTree s
pathMap f (Directory path status contents) =
    Directory (f path) status (map (pathMap f) contents)
pathMap f (RegularFile  path info) = RegularFile (f path) info
pathMap f (Symlink      path info) = Symlink     (f path) info
pathMap f (Unsupported  path info) = Unsupported (f path) info

relativizePaths :: FilePath -> FileTree s -> FileTree s
relativizePaths srcDir = pathMap stripSrcDir
  where stripSrcDir = (\(Just path) -> path) . (stripPrefix srcDir)

lStatTree :: FilePath -> IO (FileTree PF.FileStatus)
lStatTree path = do
    status <- PF.getSymbolicLinkStatus path
    if isDirectory status then do
        rawContentsNames <- getDirectoryContents path
        let contentsNames = filter (`notElem` [".", ".."]) rawContentsNames
        contents <- mapM lStatTree (map (path //) contentsNames)
        return $ Directory path status contents
    else if isRegularFile status then
        return $ RegularFile path status
    else if isSymbolicLink status then
        return $ Symlink path status
    else
        return $ Unsupported path status

doAction :: (FileStatus s) => JobSpec -> (Action s) -> IO ()
doAction spec (MkDir path status contents) = do
    let path' = dest spec // path
    createDirectoryIfMissing True path'
    syncMetadata path' status
    mapM_ (doAction spec) contents
doAction spec (MkSymlink path status) = do
    target <- PF.readSymbolicLink (src spec // path)
    PF.createSymbolicLink target (dest spec // path)
    syncMetadata (dest spec // path) status
doAction spec (DedupCopy path status) = do
    changed <- case prev spec of
        Nothing -> return True
        Just prevpath -> do
            let prevpath' = prevpath // path
            exists <- doesFileExist prevpath'
            if exists then do
                prevstatus <- PF.getSymbolicLinkStatus prevpath'
                return $ not (isRegularFile prevstatus) ||
                           (modificationTime prevstatus < modificationTime status)
            else return True
    if changed
      then do
        let srcpath = src spec // path
        file <- B.readFile srcpath
        let blobname = blobs spec // unpack (Hex.encode $ SHA1.hashlazy file)
        -- TODO: We should do this in one step by opening blobname with
        -- O_CREATE | O_EXCL, which will eliminate a race condition. We don't
        -- actually guarantee anything about concurrency safety, but it would
        -- make things more robust:
        have <- doesFileExist blobname
        unless have $ B.readFile srcpath >>= B.writeFile blobname
        PF.createLink blobname (dest spec // path)
      else do
        let Just prevpath = prev spec
        PF.createLink (prevpath // path) (dest spec // path)
    syncMetadata (dest spec // path) status
doAction _ (Report msg) = putStrLn msg



mkAction :: (FileTree s) -> Action s
mkAction (Directory path status contents) =
    MkDir path status (map mkAction contents)
mkAction (Symlink path status) = MkSymlink path status
mkAction (RegularFile path status) = DedupCopy path status
mkAction (Unsupported path _) =
    Report $ "Ignoring file of unsupported type: " ++ show path

doBackup :: JobSpec -> IO ()
doBackup spec = do
    let srcDir = src spec
    srcTree <- lStatTree srcDir
    let relativeTree = relativizePaths srcDir srcTree
    let action = mkAction relativeTree
    doAction spec action

syncMetadata :: (FileStatus s) => FilePath -> s -> IO ()
syncMetadata path status = do
    PF.setSymbolicLinkOwnerAndGroup path (fileOwner status) (fileGroup status)
    unless (isSymbolicLink status) $ do
        -- These act on the underlying file, and there are no symlink
        -- equivalents.
        PF.setFileMode path (fileMode status)
        PF.setFileTimes path (accessTime status) (modificationTime status)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [src, dest, blobs] -> doBackup $ JobSpec src dest blobs Nothing
        [src, dest, blobs, prev] -> doBackup $ JobSpec src dest blobs (Just prev)
        _ -> putStrLn "Usage : backup <src> <dest> <blobs> [ <prev> ]"
