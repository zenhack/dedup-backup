{-# LANGUAGE FlexibleInstances #-}
module Main where

import TestUtil
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Monadic
import Test.QuickCheck (arbitrary, Property)
import qualified DedupBackup as DDB
import DedupBackup ((//))

import System.Unix.Directory (withTemporaryDirectory)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)

import qualified Data.ByteString.Lazy as B
import qualified System.Posix.Files as PF

syncMetadataEq :: Property
syncMetadataEq = monadicIO $ do
    status <- pick arbitrary
    let useStatus = if DDB.isSymbolicLink status then
        -- The size of a symbolic link is the length of the name of its target,
        -- so we need to set this accordingly:
            status { size = fromIntegral $ length (testFilename ++ ".target") }
        else
            status
    let _ = status :: FileStatus
    status' <- run $ withTemporaryDirectory "testsuite.XXXXXX" (\path -> do
        let filename = path // testFilename
        if DDB.isDirectory status then
            createDirectoryIfMissing True filename
        else if DDB.isRegularFile status then
            B.writeFile
                filename
                (B.pack $ replicate (fromIntegral $ size status) 0)
        else if DDB.isSymbolicLink status then do
            B.writeFile (filename ++ ".target") (B.pack [])
            PF.createSymbolicLink (testFilename ++ ".target") filename
        else
            error "Unknown file type"
        DDB.syncMetadata filename status
        fromDDBFileStatus <$> PF.getSymbolicLinkStatus filename)
    assert $ assertSame useStatus status'
  where
    testFilename = "testfile"



writeThenReadEq :: Property
writeThenReadEq = monadicIO $ do
    tree <- pick arbitrary
    -- XXX TODO: we need this type annotation, but this is an awkward spot for
    -- it:
    let _ = tree :: (DDB.FileTree FileStatus)
    readBack <- run $ withTemporaryDirectory "testsuite.XXXXXX" (\path -> do
        writeTree (path // "src") tree
        mapStatus fromDDBFileStatus <$> DDB.lStatTree (path // "src"))
    assert $ assertSame tree readBack


main :: IO ()
main = defaultMain [ testProperty "syncMetadata path status; lstat path == status"
                                  syncMetadataEq
                   , testProperty ("Writing a file tree to disk then " ++
                                   "reading it back in yields equal trees")
                                  writeThenReadEq
                   ]
