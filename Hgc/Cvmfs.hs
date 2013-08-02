{-
Handles CVMFS commands.
-}
module Hgc.Cvmfs where
  import System.Cmd (rawSystem)
  import System.IO (hPutStr, stderr)
  import System.Exit (ExitCode(..))
  import Control.Exception (catch)
  import Control.Monad (when)
  import System.Log.Logger

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
  abort repo = rawSystem "cvmfs_server" ["abort", "-f", repo] >>= \ex -> case ex of
    ExitSuccess -> return ()
    ExitFailure r -> ioError . userError $ "Cannot abort transaction (exit code " ++ show r ++ ")."

  inTransaction :: String -- ^ Repository name
                -> Bool -- ^ Automatically publish?
                -> IO a
                -> IO a
  inTransaction repo pub action = catch op 
    (\e -> abort repo >> 
      errorM "hgc-version.cvmfs" "Failure! Attempting to abort transaction." >> 
      ioError e)
    where op = transaction repo >>
                action >>= \result -> 
                when pub (publish repo) >>
                return result