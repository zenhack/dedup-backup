{-# LANGUAGE FlexibleInstances #-}
module Main where

import TestUtil
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Monadic
import Test.QuickCheck (arbitrary, Property)
import qualified DedupBackup
import DedupBackup ((//))

import System.Unix.Directory (withTemporaryDirectory)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)

readThenWriteEq :: Property
readThenWriteEq = monadicIO $ do
    tree <- pick arbitrary
    -- XXX TODO: we need this type annotation, but this is an awkward spot for
    -- it:
    let _ = tree :: (DedupBackup.FileTree FileStatus)
--    readBack <- run $ withTemporaryDirectory "testsuite.XXXXXX" (\path -> do
 --   readBack <- run $ do
    ok <- run $ do
        let path = "/tmp/foo/bar"
        createDirectoryIfMissing True path
        writeTree (path // "src") tree
        readBack <- DedupBackup.lStatTree (path // "src")
        let ok = sameTree tree readBack
        if ok then
            removeDirectoryRecursive path
        else
            return ()
        return ok
    assert ok
--    assert $ sameTree tree readBack


main :: IO ()
main = defaultMain [ testProperty ("Writing a file tree to disk then " ++
                                   "reading it back in yields equal trees")
                                  readThenWriteEq
                   ]
