{-
Handles CVMFS commands.
-}
module Hgc.Cvmfs where
  import System.Process (rawSystem)
  import System.Exit (ExitCode(..))

  -- | CVMFS Base directory
  base :: FilePath
  base = "/cvmfs"

  -- | Start a CVMFS transaction
  transaction :: String -> IO ()
  transaction repo = rawSystem "cvmfs_server" ["transaction", repo] >>= \ex -> case ex of
    ExitSuccess -> return ()
    ExitFailure r -> ioError . userError $ "Cannot start transaction (exit code " ++ show r ++ ")."

  -- | Publish a CVMFS repository
  publish :: String -> IO ()
  publish repo = rawSystem "cvmfs_server" ["publish", repo] >>= \ex -> case ex of
    ExitSuccess -> return ()
    ExitFailure r -> ioError . userError $ "Cannot publish repo (exit code " ++ show r ++ ")."
