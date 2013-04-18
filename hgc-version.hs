{-
-- Creates a new version of a template.
hg-version <oldname> [<newname>]

- If <newname> not set, increment a version number
- Get repo
- cvmfs_server transaction repo
- Copy newname to oldname
- Copy config file to temporary location
- Modify temporary config file to adjust location of rootfs and fstab
- lxc-start and lxc-console
- Clean (remove cache dirs)
- Publish (possibly - maybe prompt?)
-}

module Main where
  import Data.Maybe (fromMaybe)
  import Data.List (intercalate)
  import Data.List.Split (splitOneOf)
  import System.Console.GetOpt
  import System.Environment (getArgs)

  import Hgc.Cvmfs

  data Options = Options {
      optPublish :: Bool -- ^ Automatically publish the CVMFS repo
    , optMajorRevision :: Bool -- ^ Create a new major revision
    , optNewCapsule :: Maybe String -- ^ Name for new capsule
    , optRepository :: String
  }

  defaultOptions = Options {
      optPublish = False
    , optMajorRevision = False
    , optNewCapsule = Nothing
    , optRepository = "mercury.repo"
  }

  options :: [OptDescr (Options -> Options)]
  options =
    [
        Option ['y'] ["publish"] (NoArg (\o -> o { optPublish = True })) 
          "Automatically publish new version."
      , Option ['M'] ["major"] (NoArg (\o -> o { optMajorRevision = True })) 
          "Create a major revision."
      , Option ['n'] ["new-capsule"] (ReqArg (\n o -> o { optNewCapsule = Just n }) "NAME") 
          "New capsule name."
      , Option ['r'] ["repository"] (ReqArg (\n o -> o { optRepository = n }) "REPOSITORY")
          "Repository name (defaults to mercury.repo)"
    ]

  usage :: String
  usage = usageInfo header options
    where header = "Usage: hgc-version [Option...] template"

  main :: IO ()
  main = do
    args <- getArgs
    case (getOpt Permute options args) of
      (o,[f],[]) -> doStuff f (foldl (flip id) defaultOptions o)
      (_,_,errs) -> putStrLn (concat errs ++ usage)

  doStuff :: String
          -> Options
          -> IO ()
  doStuff capsule opts = putStrLn $ capsuleName capsule opts

  -- TODO: replace with regex?
  capsuleName :: String -- ^ Old name
              -> Options -- ^ options
              -> String -- ^ New name
  capsuleName old opts = newname ++ "-" ++ show major ++ "." ++ show minor
    where (oldname, oldmaj, oldmin) = case reverse (splitOneOf "-." old) of
            [name] -> (name, 0, 0)
            (minor : major : name) -> 
              (intercalate "-" . reverse $ name, read major :: Int, read minor :: Int)
          newname = case optNewCapsule opts of
            Just n -> n
            Nothing -> oldname
          (major, minor) = if optMajorRevision opts then (oldmaj + 1, 0) else (oldmaj, oldmin + 1)
          
