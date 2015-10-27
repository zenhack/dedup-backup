{-# LANGUAGE FlexibleInstances #-}
module TestUtil where

import Data.UnixTime (UnixTime(..), toEpochTime)
import qualified System.Posix.Files.ByteString as PFB
import qualified System.Posix.Types as PT
import qualified System.Posix.User as PU
import qualified Main
import Main ((//))
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

data FileType = Directory
              | RegularFile
              | Symlink
              deriving(Eq)

typeMode :: FileType -> PT.FileMode
typeMode typ = case typ of
    Directory -> PFB.directoryMode
    RegularFile -> PFB.regularFileMode
    Symlink -> PFB.symbolicLinkMode


instance Arbitrary FileType where
    arbitrary = oneof $ map return [Directory, RegularFile, Symlink]

data FileStatus = FileStatus { mode  :: PT.FileMode
                             , owner :: PT.UserID
                             , group :: PT.GroupID
                             , atime :: PT.EpochTime
                             , ctime :: PT.EpochTime
                             }

instance Arbitrary FileStatus where
    arbitrary =
        FileStatus <$>     return PFB.ownerModes
                       <*> return effectiveUID
                       <*> return effectiveGID
                       <*> return epoch1
                       <*> return epoch1

mkStatus ty = do
    status <- arbitrary
    return $ status { mode = PFB.unionFileModes (mode status)
                                                (typeMode ty) }

hasMode :: PT.FileMode -> PT.FileMode -> Bool
hasMode all check = PFB.intersectFileModes all check == check

instance Main.FileStatus FileStatus where
    isRegularFile s  = hasMode (mode s) PFB.regularFileMode
    isDirectory s    = hasMode (mode s) PFB.directoryMode
    isSymbolicLink s = hasMode (mode s) PFB.symbolicLinkMode
    fileMode         = mode
    fileOwner        = owner
    fileGroup        = group
    accessTime       = atime
    modificationTime = ctime

sampleFileNames = map (:[]) ['a'..'z']
mkName = oneof $ map return sampleFileNames

instance Arbitrary (Main.FileTree FileStatus) where
    arbitrary = oneof [ do name <- mkName
                           contents <- arbitrary
                           status <- mkStatus Directory
                           let contents' = map (Main.pathMap (name //)) contents
                           return $ Main.Directory name status contents'
                      , Main.RegularFile <$> mkName <*> mkStatus RegularFile
                      , Main.Symlink <$> mkName <*> mkStatus Symlink
                      ]
