{-
Copyright (c) 2013, Genome Research Limited
Author: Nicholas Clarke <nicholas.clarke@sanger.ac.uk>

Basic support for different union file systems. 
-}
module Hgc.Union where
  import Text.Printf

  data Union = Union {
      name :: String
      -- | Format the options for this particular union system 
    , format :: FilePath -- ^ 'Lower' data directory (ro)
             -> FilePath -- ^ 'Upper' data (scratch, rw) directory
             -> String -- ^ Formatted options line, suitable for '-o' or FSTAB
  }

  aufs :: Union
  aufs = Union {
      name = "aufs"
    , format = \u l -> printf "dirs=%s:%s" l u
  }

  overlayfs :: Union
  overlayfs = Union {
      name = "overlayfs"
    , format = printf "lowerdir=%s,upperdir=%s"
  }