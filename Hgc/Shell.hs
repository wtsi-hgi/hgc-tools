{-
Handles miscellaneous other shell commands.
-}

module Hgc.Shell (
      cp
    , touch
    , mkdir
    , safeSystem
    , setSafeEnv
  ) where
  import Control.Monad (liftM, void)
  import System.Exit
  import System.Directory
  import System.Posix.Env
  import System.Posix.Files
  import System.Posix.Types (FileMode())
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
      p = (proc command args) { env = Just safeEnv }
      

  -- | Set a safe environment for the currently running program.
  --   Sets clean env, resets umask
  setSafeEnv :: IO ()
  setSafeEnv = do
    oldEnv <- liftM (map (\(a,_) -> a)) $ getEnvironment
    mapM_ unsetEnv oldEnv
    mapM_ (\(a,b) -> setEnv a b True) safeEnv
    void $ setFileCreationMask safeUmask

  safeEnv :: [(String, String)]
  safeEnv = [
          ("SHELL", "/bin/bash")
        , ("PATH", "/usr/local/bin:/usr/bin:/bin")
        , ("LANG", "en_GB.UTF-8")
        , ("IFS", "")
        ]

  safeUmask :: FileMode
  safeUmask = foldl unionFileModes nullFileMode [
      groupWriteMode
    , otherWriteMode
    ]