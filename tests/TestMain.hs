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

readThenWriteEq :: Property
readThenWriteEq = monadicIO $ do
    tree <- pick arbitrary
    -- XXX TODO: we need this type annotation, but this is an awkward spot for
    -- it:
    let _ = tree :: (DedupBackup.FileTree FileStatus)
    readBack <- run $ withTemporaryDirectory "testsuite.XXXXXX" (\path -> do
        writeTree (path // "src") tree
        DedupBackup.lStatTree (path // "src"))
    assert $ sameTree tree readBack


main :: IO ()
main = defaultMain [ testProperty "???" readThenWriteEq ]
