import System.Posix.Files
import System.Directory (getDirectoryContents, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Control.Monad (liftM, forM, void, unless)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base16 as Hex
import Data.ByteString.Char8 (unpack)
import System.Environment

getFileTreeStatus :: FilePath -> IO [(FilePath, FileStatus)]
getFileTreeStatus path = do
    status <- getSymbolicLinkStatus path
    if isDirectory status then do
        contentsNames <- liftM (map (path </>) . filter (`notElem` [".", ".."])) (getDirectoryContents path)
        contents <- liftM concat $ mapM getFileTreeStatus contentsNames
        return $ (path, status):contents
    else
        return [(path, status)]

allDirs :: [(FilePath, FileStatus)] -> [FilePath]
allDirs = map fst . filter (isDirectory . snd)

-- | doBackup src dest blobs
-- makes a backup of src at dest, using blobs as the blob directory.
doBackup :: FilePath -> FilePath -> FilePath -> IO ()
doBackup src dest blobs = do
    files <- getFileTreeStatus src
    let dirs = map dropRoot (allDirs files) in
        void $ forM dirs $ \dir ->
            createDirectoryIfMissing True (dest </> dir)
    let regFiles = map fst $ filter (isRegularFile . snd) files in
        void $ forM regFiles $ \filename -> do
            file <- B.readFile filename
            let blobname = blobs </> unpack  (Hex.encode $ SHA1.hashlazy file) in do
                have <- doesFileExist blobname
                unless have $ B.readFile filename >>= B.writeFile blobname
                createLink blobname (dest </> dropRoot filename)
    let links = map fst $ filter (isSymbolicLink . snd) files in
        void $ forM links $ \link -> do
            target <- readSymbolicLink link
            createSymbolicLink target (dest </> dropRoot link)
 where
    dropRoot = drop (length src)
    forType files pred fn = let files' = map fst $ filter (pred . snd) files in
        void $ forM files' fn

main :: IO ()
main = do
    args <- getArgs
    case args of
        [src, dest, blobs] -> doBackup src dest blobs
        _ -> putStrLn "Usage : backup <src> <dest> <blobs>"
