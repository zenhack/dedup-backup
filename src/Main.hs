module Main (main) where

import DedupBackup (JobSpec(..), doBackup)
import Options.Applicative


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
    doBackup spec
