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
module DedupBackup where

import qualified Data.Map.Strict as M
import qualified System.Posix.Files as PF
import qualified System.Posix.Types as PT
import qualified System.Posix.IO as PIO
import System.IO (hClose)
import System.Directory (getDirectoryContents, createDirectoryIfMissing, doesFileExist)
import Control.Exception (try, IOException)
import Control.Monad (liftM, forM_, mapM_, unless, when)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base16 as Hex
import Data.ByteString.Char8 (unpack)
import System.Environment
import Data.List (stripPrefix)


dedupCutOff = 128 -- | number of bytes under which a file is just copied wholesale.
                  -- Anything bigger than this gets the fancy dedup treatment.


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
    fileSize         :: a -> PT.FileOffset

instance FileStatus PF.FileStatus where
    isRegularFile    = PF.isRegularFile
    isDirectory      = PF.isDirectory
    isSymbolicLink   = PF.isSymbolicLink
    fileMode         = PF.fileMode
    fileOwner        = PF.fileOwner
    fileGroup        = PF.fileGroup
    accessTime       = PF.accessTime
    modificationTime = PF.modificationTime
    fileSize         = PF.fileSize


-- // from System.FilePath *almost* does what we want, but it drops left if
-- right starts with a slash.
(//) left right = left ++ "/" ++ right

data JobSpec = JobSpec { src   :: FilePath
                       , dest  :: FilePath
                       , blobs :: FilePath
                       , chown :: Bool
                       , prev  :: Maybe FilePath
                       }

data FileTree s = Directory s (M.Map FilePath (FileTree s))
                | RegularFile s
                | Symlink s
                | Unsupported s
                deriving(Show,Eq)

data Action s = MkDir s (M.Map FilePath (Action s))
              | MkSymlink s
              | DedupCopy s
              | NaiveCopy s
              | Report String


reportError :: IO () -> IO ()
-- ^ @reportError io@ executes @io@. if @io@ raises an @IOException@, the
-- exception is caught and printed, before resuming normal operation.
reportError io = do
    result <- (try io) :: IO (Either IOException ())
    case result of
        Left err -> print err
        Right () -> return ()

-- | @getContentNames@ is like @getDirectoryContents@, except that it excludes
-- "." and "..".
getContentsNames :: FilePath -> IO [FilePath]
getContentsNames path =
    filter (`notElem` [".", ".."]) <$> getDirectoryContents path

lStatTree :: FilePath -> IO (FileTree PF.FileStatus)
lStatTree path = do
    status <- PF.getSymbolicLinkStatus path
    if isDirectory status then do
        contentsNames <- getContentsNames path
        contents <- mapM (lStatTree . (path //))  contentsNames
        return $ Directory
                    status
                    (M.fromList (zip contentsNames contents))
    else if isRegularFile status then
        return $ RegularFile status
    else if isSymbolicLink status then
        return $ Symlink status
    else
        return $ Unsupported status

doAction :: (FileStatus s) => JobSpec -> Action s -> IO ()
doAction spec (MkDir status contents) = reportError $ do
    let path = dest spec
    createDirectoryIfMissing True path
    syncMetadata (chown spec) path status
    forM_ (M.toList contents)
          (\(path', tree) ->
                doAction
                    spec { dest = dest spec // path'
                         , src = src spec // path'
                         , prev = fmap (// path') (prev spec)
                         }
                    tree)
doAction spec (MkSymlink status) = reportError $ do
    target <- PF.readSymbolicLink (src spec)
    PF.createSymbolicLink target (dest spec)
    syncMetadata (chown spec) (dest spec) status
doAction spec (DedupCopy status) = reportError $ do
    changed <- case prev spec of
        Nothing -> return True
        Just prevpath -> do
            exists <- doesFileExist prevpath
            if exists then do
                prevstatus <- PF.getSymbolicLinkStatus prevpath
                return $ not (isRegularFile prevstatus) ||
                           (modificationTime prevstatus < modificationTime status)
            else return True
    if changed
      then do
        file <- B.readFile (src spec)
        let blobname = blobs spec // unpack (Hex.encode $ SHA1.hashlazy file)
        (try :: IO a -> IO (Either IOException a)) $ do
            fd <- PIO.openFd
                    blobname
                    PIO.WriteOnly
                    (Just $ PT.CMode 0600)
                    PIO.defaultFileFlags { PIO.exclusive = True }
            hndl <- PIO.fdToHandle fd
            B.readFile (src spec) >>= B.hPut hndl
            hClose hndl
        PF.createLink blobname (dest spec)
      else do
        let Just prevpath = prev spec
        PF.createLink prevpath (dest spec)
    syncMetadata (chown spec) (dest spec) status
doAction spec (NaiveCopy status) = reportError $ do
    B.readFile (src spec) >>= B.writeFile (dest spec)
    syncMetadata (chown spec) (dest spec) status
doAction spec (Report msg) = putStrLn (msg ++ show (src spec))



mkAction :: (FileStatus s) => FileTree s -> Action s
mkAction (Directory status contents) =
    MkDir status (M.map mkAction contents)
mkAction (Symlink status) = MkSymlink status
mkAction (RegularFile status) =
    if fileSize status > dedupCutOff then
        DedupCopy status
    else
        NaiveCopy status
mkAction (Unsupported _) =
    Report "Ignoring file of unsupported type: "

doBackup :: JobSpec -> IO ()
doBackup spec = do
    let srcDir = src spec
    putStrLn "Scanning source directory..."
    srcTree <- lStatTree srcDir
    let action = mkAction srcTree
    putStrLn "Starting backup..."
    doAction spec action

syncMetadata :: (FileStatus s) => Bool -> FilePath -> s -> IO ()
syncMetadata shouldChown path status = do
    when shouldChown $ do
        PF.setSymbolicLinkOwnerAndGroup path (fileOwner status) (fileGroup status)
    unless (isSymbolicLink status) $ do
        -- These act on the underlying file, and there are no symlink
        -- equivalents.
        PF.setFileMode path (fileMode status)
        PF.setFileTimes path (accessTime status) (modificationTime status)
