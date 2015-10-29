{-# LANGUAGE FlexibleInstances #-}
module TestMain where

import TestUtil
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Monadic
import Test.QuickCheck (arbitrary, Property)
import qualified Main
import Main ((//))

import System.Unix.Directory (withTemporaryDirectory)

readThenWriteEq :: Property
-- readThenWriteEq :: Main.FileTree FileStatus -> IO ()
readThenWriteEq = monadicIO $ do
    tree <- pick arbitrary
    return $ withTemporaryDirectory "testsuite.XXXXXX" (\path -> do
        writeTree (path // "src") tree
        readBack <- Main.lStatTree (path // "src")
        assert $ sameTree tree readBack)


main :: IO ()
main = defaultMain [ testProperty "???" readThenWriteEq ]
