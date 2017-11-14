module Main (main) where

import Options.Applicative

import ConvertVersion (ensureLatestFormat)
import Data.Monoid    ((<>))
import DedupBackup    (JobSpec(..), doBackup)


argParser :: Parser JobSpec
argParser = JobSpec <$>
    strOption
        ( long "src"
       <> short 's'
       <> metavar "SOURCE"
       <> help "Directory to back up"
        )
    <*> strOption
        ( long "dest"
       <> short 'd'
       <> metavar "DESTINATION"
       <> help "Location to store the backup"
        )
    <*> strOption
        ( long "blobs"
       <> short 'b'
       <> metavar "BLOB DIR"
       <> help "Blob directory"
        )
    <*> switch
        ( long "chown"
       <> help "Change ownership on backup (requires root)"
        )
    <*> (optional $ strOption
        ( long "prev"
       <> short 'p'
       <> metavar "PREVIOUS"
       <> help "Previous backup; If specified will use modification times for speedup."
        ))


parseArgs :: IO JobSpec
parseArgs = execParser $
    info argParser (progDesc "Backup tool with file-level deduplication.")

main :: IO ()
main = do
    spec <- parseArgs
    ensureLatestFormat (blobs spec)
    doBackup spec
