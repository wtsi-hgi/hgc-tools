{-
Handles LXC related functions.
-}
module Hgc.Lxc where
  import Control.Monad (liftM)
  import Data.Maybe (mapMaybe)
  import Data.List.Split (splitOn)
  import qualified Data.Map as Map

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

  -- | Run the LXC container and attach to it using lxc-console.
  --   Should wait until the container shuts down.
  console :: String -- ^ Name of the LXC container
          -> FilePath -- ^ Configuration file
          -> IO ()
  console = undefined

