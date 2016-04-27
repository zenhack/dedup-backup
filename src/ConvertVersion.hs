{- | Upgrade the blob directory format to the latest version

Version 0 was simply a directory of files named by their sha1 hashes
(in hexidecimal).

Version 2 splits these into subdirectories based on their first byte.
the sub-directories are named 00 through ff. A file named "version" is
present in the root of the blob directory, containing the string "1".
This file is used to determine the version of the repository. Its absence
denotes version 0.

TODO: this module does less error handling than it should.

-}
module ConvertVersion (getVersion, convert, ensureLatestFormat) where

import Control.Exception (catch, IOException)
import Control.Monad (when, forM_)
import System.Directory (createDirectoryIfMissing, getDirectoryContents)
import System.Posix.Files (rename)
import Text.Printf (printf)

currentVersion = 1 :: Int

type ExnHandler e a = IO a -> (e -> IO a) -> IO a

-- | @getVersion path@ returns the version number of the blobdir at @path@
getVersion :: FilePath -> IO Int
getVersion path = getVersion' `catchIO` (\_ -> return 0) where
    catchIO = catch :: ExnHandler IOException a
    getVersion' = parse <$> readFile (path ++ "/version")
    parse str = case reads str of
        [(x, _)]-> x
        _ -> 0

-- | @convert path@ converts the blob directory at @path@ to the latest version.
convert :: FilePath -> IO ()
convert path = do
    createSubdirs
    blobnames <- getBlobNames
    forM_ blobnames $ \name@(c1:c2:cs) -> rename
        (printf "%s/%s" path name)
        (printf "%s/%s/%s" path [c1,c2] name)
    writeFile (path ++ "/version") (show currentVersion)


  where
    getBlobNames = filter (\name -> length name == 40) <$> getDirectoryContents path
    createSubdirs = forM_ ([0,1..255] :: [Int]) $ \n -> do
        createDirectoryIfMissing False (printf "%s/%02x" path n)


-- | ensures the blobdir is at its latest version.
ensureLatestFormat :: FilePath -> IO ()
ensureLatestFormat path = do
    -- TODO: we should fail if the blobdir is more recent than currentVersion.
    version <- getVersion path
    when (version < currentVersion) (convert path)
