{-# LANGUAGE FlexibleInstances #-}
module TestUtil where

import Control.Monad (liftM, forM_)
import Data.UnixTime (UnixTime(..), toEpochTime)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import qualified System.Posix.Files.ByteString as PFB
import qualified System.Posix.Files as PF
import qualified System.Posix.Types as PT
import qualified System.Posix.User as PU
import qualified DedupBackup as DDB
import DedupBackup ((//))
import System.Directory (createDirectoryIfMissing)
import Test.Framework
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneof)

-- We want to be able to do testing as a regular user, so we'll construct our
-- examples using a UID/GID that we actually control. @unsafePerformIO@ lets
-- us have constants, rather than having to thread the IO monad through
-- everything to get a value that's never going to change.
import System.IO.Unsafe (unsafePerformIO)
{-# NOINLINE effectiveUID #-}
effectiveUID = unsafePerformIO PU.getEffectiveUserID
{-# NOINLINE effectiveGID #-}
effectiveGID = unsafePerformIO PU.getEffectiveGroupID

-- handy to have an off-the-shelf time we can use.
epoch1 = toEpochTime (UnixTime 1 0)

data FileStatus = FileStatus { mode  :: PT.FileMode
                             , owner :: PT.UserID
                             , group :: PT.GroupID
                             , atime :: PT.EpochTime
                             , ctime :: PT.EpochTime
                             , size  :: PT.FileOffset
                             } deriving(Show)

instance Arbitrary FileStatus where
    arbitrary = do
        rawStatus <- FileStatus <$>     return PFB.ownerModes
                                    <*> return effectiveUID
                                    <*> return effectiveGID
                                    <*> return epoch1
                                    <*> return epoch1
                                    <*> liftM
                                            (fromInteger . (`mod` maxFileSize))
                                            arbitrary
        typeMode <- oneof $ map return [ PFB.directoryMode
                                       , PFB.regularFileMode
                                       , PFB.symbolicLinkMode
                                       ]
        return rawStatus { mode = PFB.unionFileModes (mode rawStatus) typeMode }
      where maxFileSize = 32 * 1024

hasMode :: PT.FileMode -> PT.FileMode -> Bool
hasMode all check = PFB.intersectFileModes all check == check

instance DDB.FileStatus FileStatus where
    isRegularFile s  = hasMode (mode s) PFB.regularFileMode
    isDirectory s    = hasMode (mode s) PFB.directoryMode
    isSymbolicLink s = hasMode (mode s) PFB.symbolicLinkMode
    fileMode         = mode
    fileOwner        = owner
    fileGroup        = group
    accessTime       = atime
    modificationTime = ctime
    fileSize         = size

sampleFileNames = map (:[]) ['a'..'z']
mkName = oneof $ map return sampleFileNames

-- | Write the given tree out to the filesystem. Unlike the effects of
-- @doAction@, this doesn't require an existing file tree somewhere else.
-- This makes it possible to use @writeTree@ with a randomly generated
-- value (from quickcheck).
--
-- Note that there is some missing information that is normally pulled from the
-- src directory. We fill it in as follows:
--
-- * The contents of the file are all zeros, with a size matching that
--   specified by the status.
-- * Symlinks always link to "..".
writeTree :: (DDB.FileStatus s) => FilePath -> DDB.FileTree s -> IO ()
writeTree path (DDB.Directory status contents) = do
    createDirectoryIfMissing True path
    forM_ (M.toList contents) (\(name, subtree) ->
        writeTree (path // name) subtree)
    DDB.syncMetadata path status
writeTree path (DDB.RegularFile status) = do
    let contents = B.pack $ replicate (fromIntegral $ DDB.fileSize status) 0
    B.writeFile path contents
    DDB.syncMetadata path status
writeTree path (DDB.Symlink status) = do
    PF.createSymbolicLink ".." path
    DDB.syncMetadata path status


instance Arbitrary (DDB.FileTree FileStatus) where
    arbitrary = do
        status <- arbitrary
        if DDB.isDirectory status then do
            contents <- arbitrary
            return $ DDB.Directory
                        status
                        (M.fromList (zip sampleFileNames contents))
        else if DDB.isRegularFile status then
            return $ DDB.RegularFile status
        else if DDB.isSymbolicLink status then
            return $ DDB.Symlink status
        else
            error "BUG: Unrecogized file type!"

sameTree :: (DDB.FileStatus a, DDB.FileStatus b) =>
    DDB.FileTree a -> DDB.FileTree b -> Bool
sameTree (DDB.Symlink s1)      (DDB.Symlink s2)      = statusEq s1 s2
sameTree (DDB.RegularFile s1)  (DDB.RegularFile s2)  = statusEq s1 s2
sameTree (DDB.Directory s1 c1) (DDB.Directory s2 c2) =
    and $ (statusEq s1 s2):(zipWith sameContents (M.toAscList c1) (M.toAscList c2))
  where
    sameContents (k1, v1) (k2, v2) = k1 == k2 && (sameTree v2 v1)
sameTree _ _ = False

statusEq :: (DDB.FileStatus a, DDB.FileStatus b) => a -> b -> Bool
statusEq l r = and [ DDB.fileMode l         == DDB.fileMode r
                   , DDB.fileOwner l        == DDB.fileOwner r
                   , DDB.fileGroup l        == DDB.fileGroup r
                   , DDB.accessTime l       == DDB.accessTime r
                   , DDB.modificationTime l == DDB.modificationTime r
                   , DDB.fileSize l         == DDB.fileSize r
                   ]
