{-
Handles LXC related functions.
-}
module Hgc.Lxc where
  import Control.Monad (liftM)
  import Data.Maybe (mapMaybe)
  import Data.List.Split (splitOn)
  import qualified Data.Map as Map

  import System.Process
  import System.Exit

  type Config = Map.Map String String

  -- | Read a configuration file into memory.
  -- Ignores any comment lines or lines that aren't configuration.
  readConfig :: FilePath -- ^ Configuration file
             -> IO Config
  readConfig fp = readFile fp >>=
    return . Map.fromList . mapMaybe (\a ->
      case splitOn "=" a of
        key : val : [] -> Just (key,val)
        _ -> Nothing
    ) . filter (\a -> head a == '#') . lines

  -- | Write a configuration file.
  writeConfig :: FilePath -- ^ Config location
              -> Config -- ^ Config 
              -> IO ()
  writeConfig fp config = writeFile fp str where
    str = unlines . map (\(a,b) -> a ++ "=" ++ b) . Map.toList $ config

  -- | Set a config option
  setConfig :: String -- ^ Key
            -> String -- ^ Value
            -> Config
            -> Config
  setConfig = Map.insert

  -- | Run the LXC container and attach to it using lxc-console.
  --   Should wait until the container shuts down.
  console :: String -- ^ Name of the LXC container
          -> FilePath -- ^ Configuration file
          -> IO ()
  console capsule config = 
    let start = rawSystem "lxc-start" ["-n", capsule, "-f", config, "-d"] >>= \s -> case s of
                  ExitSuccess -> return ()
                  ExitFailure r -> ioError . userError $ 
                    "Cannot start capsule (exit code " ++ show r ++ ")."
        console' = rawSystem "lxc-console" ["-n", capsule, "-t", "1"] >>= \s -> case s of
                  ExitSuccess -> return ()
                  ExitFailure r -> ioError . userError $ 
                    "Cannot start console (exit code " ++ show r ++ ")."
    in start >> console'