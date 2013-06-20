{-
Handles LXC related functions.
-}
module Hgc.Lxc 
    ( readConfig
    , writeConfig
    , setConfig
    , addConfig
    , console
    )
  where
  import Control.Monad (liftM)
  import Data.Maybe (mapMaybe)
  import Data.List.Split (splitOn)
  import Data.Char (isSpace)
  import Data.List (isPrefixOf, sortBy)
  import qualified Data.Map as Map

  import Debug.Trace

  import System.Process
  import System.Exit

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
    ) . filter (\a -> not $ isPrefixOf "#" a) . map (dropWhile isSpace) . lines

  -- | Sort the config items (some of the network stuff relies on order!)
  sortConfigItem (k1, _) (k2, _)
    | p1 < p2 = LT
    | p1 >= p2 = GT
    where 
      p1 = p k1
      p2 = p k2
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
  writeConfig fp config = writeFile fp str where
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