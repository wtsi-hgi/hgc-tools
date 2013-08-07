{-
Handles miscellaneous other shell commands.
-}

module Hgc.Shell (
      cp
    , touch
    , mkdir
    , safeSystem
  ) where 
  import System.Exit
  import System.Directory
  import System.Process

  cp :: FilePath -> FilePath -> IO ExitCode
  cp src dest = safeSystem "cp" ["-r", src, dest]

  touch :: FilePath -> IO ExitCode
  touch f = safeSystem "touch" [f]

  mkdir :: FilePath -> IO ()
  mkdir = createDirectoryIfMissing True

  -- | Execute an external process in a 'safe' way.
  safeSystem :: String -> [String] -> IO ExitCode
  safeSystem command args = 
    createProcess p >>= \(_,_,_,ph) ->
    waitForProcess ph
    where
      p = (proc command args) { env = safeEnv }
      safeEnv = Just [
          ("SHELL", "/bin/bash")
        , ("PATH", "/usr/local/bin:/usr/bin:/bin")
        , ("LANG", "en_GB.UTF-8")
        , ("IFS", "")
        ]