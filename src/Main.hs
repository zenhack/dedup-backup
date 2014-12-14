{-
    Copyright 2014 Ian Denhardt <ian@zenhack.net>

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
import System.Posix.Files
import System.Directory (getDirectoryContents, createDirectoryIfMissing, doesFileExist)
import Control.Monad (liftM, forM_, unless)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base16 as Hex
import Data.ByteString.Char8 (unpack)
import System.Environment
import Data.List (stripPrefix)

-- // from System.FilePath *almost* does what we want, but it drops left if
-- right starts with a slash.
(//) left right = left ++ "/" ++ right

lStatTree :: FilePath -> IO [(FilePath, FileStatus)]
lStatTree path = do
    status <- getSymbolicLinkStatus path
    if isDirectory status then do
        contentsNames <- liftM (map (path //) . filter (`notElem` [".", ".."])) (getDirectoryContents path)
        contents <- liftM concat $ mapM lStatTree contentsNames
        return $ (path, status):contents
    else
        return [(path, status)]

-- | doBackup src dest blobs
-- makes a backup of src at dest, using blobs as the blob directory.
doBackup :: FilePath -> FilePath -> FilePath -> IO ()
doBackup src dest blobs = do
    files <- lStatTree src

    forType files isDirectory $ \dir ->
        createDirectoryIfMissing True (dest // stripSrc dir)

    forType files isRegularFile $ \filename -> do
        file <- B.readFile filename
        let blobname = blobs // unpack  (Hex.encode $ SHA1.hashlazy file)
        have <- doesFileExist blobname
        unless have $ B.readFile filename >>= B.writeFile blobname
        createLink blobname (dest // stripSrc filename)

    forType files isSymbolicLink $ \link -> do
        target <- readSymbolicLink link
        createSymbolicLink target (dest // stripSrc link)
 where
    stripSrc path = let Just suffix = stripPrefix src path in suffix
    forType files pred fn = let files' = map fst $ filter (pred . snd) files in
        forM_ files' fn

main :: IO ()
main = do
    args <- getArgs
    case args of
        [src, dest, blobs] -> doBackup src dest blobs
        _ -> putStrLn "Usage : backup <src> <dest> <blobs>"
