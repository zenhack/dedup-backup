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

    dedup-backup <src> <dest> <blobs> [ <prev> ]

`<src>` is the directory to back up. `<dest>` is the location of the
backup. `<blobs>` is a directory used to store files by cryptographic
hash; it is used for de-duplication. This directory should be the same
for all backups, and it *must* be on the same partition as `<dest>`.
`<prev>` is an (optional) previous backup of the same `<src>` If
specified, the modification times of the files in `<prev>` and the
current version of `<src>` will be used to speed up the backup --- if
the modification times are the same the file is assumed to be unchanged,
and will not be hashed.

# License

GPL 3.0 or later.
