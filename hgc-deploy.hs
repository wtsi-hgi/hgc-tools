{-
Rewrite of hgc-deploy in Haskell, aiming to be more configurable and maintainable.
-}

module Main where
  import System.Console.GetOpt
  import System.Environment (getArgs)

  data Options = Options {
      optRepository :: String
    , optMount :: [FilePath] -- ^ Resources to mount in the capsule.
    , optVerbose :: Bool
  }

  defaultOptions :: Options
  defaultOptions = Options {
      optRepository = "mercury.repo"
    , optMount = []
    , optVerbose = False
  }

  options :: [OptDescr (Options -> Options)]
  options =
    [
        Option ['m'] ["mount"] (ReqArg (\n o -> o { optMount = n : optMount o }) "MOUNT_POINT")
          "Mount the specified resource into the capsule."
      , Option ['r'] ["repository"] (ReqArg (\n o -> o { optRepository = n }) "REPOSITORY")
          "Use the specified repository name (defaults to mercury.repo)."
      , Option ['v'] ["verbose"] (NoArg (\o -> o  { optVerbose = True }))
          "Enable verbose output."
    ]

  usage :: String
  usage = usageInfo header options
    where header = "Launch a Mercury capsule.\n" ++
                    "Usage: hgc-deploy [Option...] capsule"

  main :: IO ()
  main = do
    args <- getArgs
    case (getOpt Permute options args) of
      (o,[f],[]) -> deploy f (foldl (flip id) defaultOptions o)
      (_,_,errs) -> putStrLn (concat errs ++ "\n" ++ usage)

  deploy :: String -- ^ Capsule
         -> Options -- ^ Options
         -> IO ()
  deploy = undefined