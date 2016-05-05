{-# LANGUAGE FlexibleInstances #-}
module Main where

import TestUtil
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Monadic
import Test.QuickCheck (arbitrary, Property)
import qualified DedupBackup as DDB
import DedupBackup ((//))
import ConvertVersion (ensureLatestFormat)

import System.Unix.Directory (withTemporaryDirectory)
import System.Directory (createDirectoryIfMissing)

import qualified Data.ByteString.Lazy as B
import qualified System.Posix.Files as PF
import qualified Data.Map.Strict as M

syncMetadataEq :: Property
syncMetadataEq = monadicIO $ do
    status <- pick arbitrary
    shouldChown <- pick arbitrary
    let useStatus = if DDB.isSymbolicLink status then
        -- The size of a symbolic link is the length of the name of its target,
        -- so we need to set this accordingly:
            status { size = fromIntegral $ length (testFilename ++ ".target") }
        else
            status
    let _ = status :: FileStatus
    status' <- run $ withTemporaryDirectory testPath (\path -> do
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
        DDB.syncMetadata shouldChown filename status
        fromDDBFileStatus <$> PF.getSymbolicLinkStatus filename)
    assert $ assertSame useStatus status'
  where
    testFilename = "testfile"


writeThenReadEq :: Property
writeThenReadEq = monadicIO $ do
    tree <- pick arbitrary
    shouldChown <- pick arbitrary
    -- XXX TODO: we need this type annotation, but this is an awkward spot for
    -- it:
    let _ = tree :: (DDB.FileTree FileStatus)
    readBack <- run $ withTemporaryDirectory testPath (\path -> do
        writeTree shouldChown (path // "src") tree
        lStatTree (path // "src"))
    assert $ assertSame tree readBack

copyEq :: Property
copyEq = monadicIO $ do
    tree <- pick arbitrary
    shouldChown <- pick arbitrary
    let _ = tree :: (DDB.FileTree FileStatus)
    run $ withTemporaryDirectory testPath (\path -> do
        writeTree shouldChown (path // "src") tree
        createDirectoryIfMissing True (path // "blobs")
        ensureLatestFormat (path // "blobs")
        DDB.doBackup DDB.JobSpec { DDB.src   = path // "src"
                                 , DDB.dest  = path // "dest"
                                 , DDB.blobs = path // "blobs"
                                 , DDB.chown = shouldChown
                                 , DDB.prev  = Nothing
                                 }
        srcTree  <- lStatTree (path // "src")
        destTree <- lStatTree (path // "dest")
        return $ assertSame srcTree destTree)

cTimeCopyEq :: Property
cTimeCopyEq = monadicIO $ do
    tree <- pick arbitrary
    patch <- pick arbitrary
    shouldChown <- pick arbitrary
    let _ = tree :: (DDB.FileTree FileStatus)
    run $ withTemporaryDirectory testPath (\path -> do
        writeTree shouldChown (path // "src") tree
        createDirectoryIfMissing True (path // "blobs")
        ensureLatestFormat (path // "blobs")
        createDirectoryIfMissing True (path // "dest")
        DDB.doBackup DDB.JobSpec { DDB.src   = path // "src"
                                 , DDB.dest  = path // "dest/1"
                                 , DDB.blobs = path // "blobs"
                                 , DDB.chown = shouldChown
                                 , DDB.prev  = Nothing
                                 }
        srcTree1 <- lStatTree (path // "src")
        applyPatch shouldChown patch (path // "src")
        srcTree2 <- lStatTree (path // "src")
        DDB.doBackup DDB.JobSpec { DDB.src   = path // "src"
                                 , DDB.dest  = path // "dest/2"
                                 , DDB.blobs = path // "blobs"
                                 , DDB.chown = shouldChown
                                 , DDB.prev  = Just (path // "dest/1")
                                 }
        destTree1 <- lStatTree (path // "dest/1")
        destTree2 <- lStatTree (path // "dest/2")
        return $ assertSame srcTree1 destTree1
        return $ assertSame srcTree2 destTree2)


sizeCutOffActions :: DDB.FileTree FileStatus -> Bool
sizeCutOffActions tree = case (DDB.mkAction tree, tree) of
    (DDB.NaiveCopy _, DDB.RegularFile status) -> DDB.fileSize status <= DDB.dedupCutOff
    (DDB.DedupCopy _, DDB.RegularFile status) -> DDB.fileSize status >  DDB.dedupCutOff
    (_, DDB.Directory _ contents) ->
        and . (map snd) . M.toList $ M.map sizeCutOffActions contents
    _ -> True


main :: IO ()
main = defaultMain [ testProperty
                        "syncMetadata path status; lstat path == status"
                        syncMetadataEq
                   , testProperty
                        ("Writing a file tree to disk then reading it " ++
                         "back in yields equal trees")
                        writeThenReadEq
                   , testProperty
                        "Doing a backup of a clean tree makes an equal copy."
                        copyEq
                   , testProperty
                        ("Doing an incremental  backup with a prev backup " ++
                         "makes a correct copy.")
                        cTimeCopyEq
                   , testProperty
                        ("The file size cutoff for deduplication vs not is " ++
                         "respected.")
                        sizeCutOffActions
                   ]
