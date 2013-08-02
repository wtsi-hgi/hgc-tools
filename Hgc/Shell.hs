{-
Handles miscellaneous other shell commands.
-}

module Hgc.Shell where 
  import System.Cmd
  import System.Exit
  import System.Directory

  cp :: FilePath -> FilePath -> IO ExitCode
  cp src dest = rawSystem "cp" ["-r", src, dest]

  touch :: FilePath -> IO ExitCode
  touch fp = rawSystem "touch" [fp]

  mkdir :: FilePath -> IO ()
  mkdir = createDirectoryIfMissing True