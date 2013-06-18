{-
Handles CVMFS commands.
-}
module Hgc.Cvmfs where
  import System.Cmd (rawSystem)
  import System.Exit (ExitCode(..))

  -- | CVMFS Base directory
  base :: FilePath
  base = "/cvmfs"

  -- | Start a CVMFS transaction
  transaction :: String -- ^ repository name
              -> IO ()
  transaction repo = rawSystem "cvmfs_server" ["transaction", repo] >>= \ex -> case ex of
    ExitSuccess -> return ()
    ExitFailure r -> ioError . userError $ "Cannot start transaction (exit code " ++ show r ++ ")."

  -- | Publish a CVMFS repository
  publish :: String -> IO ()
  publish repo = rawSystem "cvmfs_server" ["publish", repo] >>= \ex -> case ex of
    ExitSuccess -> return ()
    ExitFailure r -> ioError . userError $ "Cannot publish repo (exit code " ++ show r ++ ")."

  -- | Abort a transaction on a CVMFS repository.
  abort :: String -- ^ repository name
        -> IO ()
  abort repo = rawSystem "cvmfs_server" ["publish", repo] >>= \ex -> case ex of
    ExitSuccess -> return ()
    ExitFailure r -> ioError . userError $ "Cannot abort transaction (exit code " ++ show r ++ ")."