{-
Copyright (c) 2013, Genome Research Limited
Author: Nicholas Clarke <nicholas.clarke@sanger.ac.uk>

Handles LXC related functions.
-}
module Hgc.Lxc 
    ( readConfig
    , writeConfig
    , setConfig
    , addConfig
    , console
    , withContainerDaemon
    , runCommand
    , startConsole
    )
  where
  import Control.Exception (bracket)
  import Data.Maybe (mapMaybe)
  import Data.List.Split (splitOn)
  import Data.Char (isSpace)
  import Data.List (isPrefixOf, sortBy)
  import qualified Data.Map as Map

  import System.Exit
  import System.Log.Logger

  import Hgc.Shell (safeSystem)

  type Config = Map.Map String [String]

  -- | Read a configuration file into memory.
  -- Ignores any comment lines or lines that aren't configuration.
  readConfig :: FilePath -- ^ Configuration file
             -> IO Config
  readConfig fp = readFile fp >>=
    return . Map.fromListWith (++) . mapMaybe (\a ->
      case splitOn "=" a of
        key : val : [] -> Just (key,[val])
        _ -> Nothing
    ) . filter (not . isPrefixOf "#") . map (dropWhile isSpace) . lines

  -- | Sort the config items (some of the network stuff relies on order!)
  sortConfigItem :: (String, t) -> (String, t1) -> Ordering
  sortConfigItem (k1, _) (k2, _)
    | p1 < p2 = LT
    | p1 > p2 = GT
    | otherwise = EQ
    where 
      p1 = p k1
      p2 = p k2
      p :: String -> Int
      p key
        | key == "lxc.utsname" = 1
        | key == "lxc.tty" = 2
        | key == "lxc.pts" = 3
        | key == "lxc.mount" = 4
        | key == "lxc.rootfs" = 4
        | key == "lxc.network.type" = 8
        | key == "lxc.network.link" = 9
        | isPrefixOf "lxc.network" key = 10
        | isPrefixOf "lxc.cgroup" key = 20
        | otherwise = 15

  -- | Write a configuration file.
  writeConfig :: FilePath -- ^ Config location
              -> Config -- ^ Config 
              -> IO ()
  writeConfig fp config = do
    debugM "hgc.lxc" $ "Writing config file to " ++ fp
    writeFile fp str where
      str = unlines . bind (\(a,b) -> map (\c -> a ++ "=" ++ c) b) . 
        sortBy sortConfigItem . Map.toList $ config
      bind = flip (>>=)

  -- | Set a config option. This overwrites all config options for that key.
  setConfig :: String -- ^ Key
            -> [String] -- ^ Value
            -> Config
            -> Config
  setConfig = Map.insert

  -- | Add a config option for a given key.
  addConfig :: String -- ^ Key
            -> String -- ^ Value
            -> Config
            -> Config
  addConfig k v c = Map.adjust (\a -> v : a) k c

  {- | Start the container and execute the given operation with it running.
       Typically you would start the console and this will ensure that the container
       gets stopped when you detach from the console.
  -}
  withContainerDaemon :: String -- ^ Name of the LXC container
            -> FilePath -- ^ Configuration file
            -> IO a -- ^ Operation to perform with the LXC container running.
            -> IO a
  withContainerDaemon capsule config f =
    let start = safeSystem "lxc-start" ["-n", capsule, "-f", config, "-d"] >>= \s -> case s of
              ExitSuccess -> return ()
              ExitFailure r -> ioError . userError $ 
                "Cannot start capsule (exit code " ++ show r ++ ")."
        stop = safeSystem "lxc-stop" ["-n", capsule]
    in bracket 
        (debugM "hgc.lxc" ("Starting LXC container " ++ capsule) >> start)
        (\_ -> debugM "hgc.lxc" ("Stopping LXC container " ++ capsule) >> stop) 
        (\_ -> f)

  {- | Run a command in the container, keeping it in the foreground. 
  -}
  runCommand :: String -- ^ Name of the LXC container
              -> FilePath -- ^ Configuration file
              -> String -- ^ Command
              -> IO ()
  runCommand capsule config command =
    safeSystem "lxc-execute" ["-n", capsule, "-f", config, "--", command] >>= \s -> case s of
      ExitSuccess -> return ()
      ExitFailure r -> ioError . userError $ 
                       "Cannot start capsule (exit code " ++ show r ++ ")."

  console :: String -- ^ Container name.
          -> Int -- ^ TTY to attach to.
          -> IO ()
  console capsule tty = 
    debugM "hgc.lxc"  ("Attaching lxc-console to " ++ capsule) >>
    safeSystem "lxc-console" ["-n", capsule, "-t", show tty] >>= \s -> case s of
      ExitSuccess -> return ()
      ExitFailure r -> ioError . userError $ 
        "Cannot start console (exit code " ++ show r ++ ")."

  -- | Run the LXC container and attach to it using lxc-console.
  --   Should wait until the container shuts down.
  --   This is not terribly safe and should be removed in favour of approaches
  --   that cleanly stop the capsule on abnormal termination.
  startConsole :: String -- ^ Name of the LXC container
          -> FilePath -- ^ Configuration file
          -> IO ()
  startConsole capsule config = 
    let start = safeSystem "lxc-start" ["-n", capsule, "-f", config, "-d"] >>= \s -> case s of
                  ExitSuccess -> return ()
                  ExitFailure r -> ioError . userError $ 
                    "Cannot start capsule (exit code " ++ show r ++ ")."
        console' = safeSystem "lxc-console" ["-n", capsule, "-t", "1"] >>= \s -> case s of
                  ExitSuccess -> return ()
                  ExitFailure r -> ioError . userError $ 
                    "Cannot start console (exit code " ++ show r ++ ")."
    in debugM "hgc.lxc" "Starting LXC console" >> start >> console'