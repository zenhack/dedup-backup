{-# LANGUAGE FlexibleInstances #-}
module TestUtil where

import Control.Monad (liftM, forM_)
import Data.UnixTime (UnixTime(..), toEpochTime)
import Data.Bits ((.&.), (.|.))
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

fromDDBFileStatus :: (DDB.FileStatus a) => a -> FileStatus
fromDDBFileStatus s = FileStatus { mode  = DDB.fileMode s
                                 , owner = DDB.fileOwner s
                                 , group = DDB.fileGroup s
                                 , atime = DDB.accessTime s
                                 , ctime = DDB.modificationTime s
                                 , size  = DDB.fileSize s
                                 }

assertSameStatus :: (DDB.FileStatus a, DDB.FileStatus b) => a -> b -> Bool
assertSameStatus s1 s2 = if s1' == s2' then True else
    error $ show s1' ++ "\n    /=\n"  ++ show s2'
  where
    s1' = fromDDBFileStatus s1
    s2' = fromDDBFileStatus s2

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
hasMode all check = all .&. check /= 0

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

-- We could make a functor instance, but it's not incredibly natural for the
-- FileStatus to be the "value" per se, and we don't really need the generality.
mapStatus :: (a -> b) -> DDB.FileTree a -> DDB.FileTree b
mapStatus f (DDB.Symlink s) = DDB.Symlink (f s)
mapStatus f (DDB.RegularFile s) = DDB.RegularFile (f s)
mapStatus f (DDB.Directory s c) = DDB.Directory (f s) (M.map (mapStatus f) c)

instance Eq FileStatus where
    l == r = and $ [ DDB.fileMode l      .&. (PFB.accessModes .|. PFB.fileTypeModes)
                        == DDB.fileMode r .&. (PFB.accessModes .|. PFB.fileTypeModes)
                   , DDB.fileOwner l        == DDB.fileOwner r
                   , DDB.fileGroup l        == DDB.fileGroup r
                   , DDB.accessTime l       == DDB.accessTime r
                   , DDB.modificationTime l == DDB.modificationTime r
                   , DDB.fileMode l .&. PFB.fileTypeModes == PFB.directoryMode
                      || DDB.fileSize l == DDB.fileSize r
                   ]
