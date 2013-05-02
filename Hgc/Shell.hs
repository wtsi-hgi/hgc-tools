{-
Handles miscellaneous other shell commands.
-}

module Hgc.Shell where 
  import System.Cmd
  import System.Exit

  cp :: FilePath -> FilePath -> IO ExitCode
  cp src dest = rawSystem "cp" ["-r", src, dest]