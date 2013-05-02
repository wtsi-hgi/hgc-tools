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
  import System.Exit (ExitCode(..))
  import System.FilePath
  import System.Random (randomIO)

  import Hgc.Cvmfs
  import Hgc.Lxc
  import Hgc.Shell

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
          (newname, major, minor) = case optNewCapsule opts of
            Just n -> (n, oldmaj, oldmin)
            Nothing -> if optMajorRevision opts 
              then (oldname, oldmaj + 1, 0) 
              else (oldname, oldmaj, oldmin + 1)

  -- Copy the old capsule to a new version location
  copyCapsule :: String -- ^ Old name
              -> String -- ^ New name
              -> FilePath -- ^ Repository location
              -> IO ()
  copyCapsule oldname newname repobase = cp oldloc newloc >>= \cpStatus ->
    case cpStatus of
      ExitSuccess -> return ()
      ExitFailure r -> ioError . userError $ "Failure to copy capsule (exit code " ++ show r ++ ")."
    where
      oldloc = repobase </> oldname
      newloc = repobase </> newname
  
  -- Copy the config file to a temporary location for modification
  copyTemporaryConfig :: FilePath -- ^ Capsule location
                      -> IO FilePath -- ^ location of the new configuration file.
  copyTemporaryConfig capsule = (randomIO :: IO Int) >>= \rand ->
    let newloc = "/tmp/config-" </> show rand
        oldloc = capsule </> "config"
    in cp oldloc newloc >>= \cpStatus ->
      case cpStatus of
        ExitSuccess -> return newloc
        ExitFailure r -> ioError . userError $ "Failure to copy to temporary config (exit code " ++ show r ++ ")."

  -- Update the config
  updateConfig :: FilePath -- Capsule location
               -> FilePath -- Config file location
               -> IO ()
  updateConfig capsule configloc = readConfig configloc >>= \c ->
    writeConfig configloc $ update c
    where update c = setConfig "lxc.rootfs" (capsule </> "rootfs") .
                     setConfig "lxc.mount"  (capsule </> "fstab") $ c

  -- Clean the template, removing specified cache directories (/var/cache/pacman etc)