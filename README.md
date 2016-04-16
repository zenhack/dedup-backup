This is a small tool I use for backups. Highlights:

* Efficiently uses:
  * Disk space - files are de-duplicated using cryptographic hash
    functions (currently sha1, but moving to a more secure hash function
    will not be difficult and is planned).
  * Bandwidth (if backing up to a remote filesystem) - it only copies
    things not already on the backup filesystem.
  * Time - It can make use of modification times and a previous backup
    to speed up incremental backups.

I use it roughly once a week to do full backups of my home directory
(about 500 GiB of data). The amount of additional space used per backup
is negligible.

It assumes a Unix-like system (for the file system semantics). It
therefore will not work on Windows.

# Installation

Via cabal, Haskell's package manager:

    git clone https://github.com/zenhack/dedup-backup
    cd dedup-backup
    cabal install

# Usage

    dedup-backup (-s|--src SOURCE) (-d|--dest DESTINATION)
                 (-b|--blobs BLOB DIR) [--chown] [-p|--prev PREVIOUS]

`SOURCE` is the directory to back up. `DESTINATION` is the location of
the backup. `BLOB DIR` is a directory used to store files by
cryptographic hash; it is used for de-duplication. This directory should
be the same for all backups, and it *must* be on the same partition as
`DESTINATION`.

`PREVIOUS` is an (optional) previous backup of the same `SOURCE` If
specified, the modification times of the files in `PREVIOUS`
and the current version of `SOURCE` will be used to speed up the backup
--- if the modification times are the same the file is assumed to be
unchanged, and will not be hashed.

The `--chown` option tells `dedup-backup` to change the owner and group
on the files in `DESTINATION` to match those in `SOURCE`. Root access
is required for this option.

# License

GPL 3.0 or later.
