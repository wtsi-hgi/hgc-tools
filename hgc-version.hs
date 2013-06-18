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
  import System.Directory (removeDirectoryRecursive)

  import qualified Hgc.Cvmfs as Cvmfs
  import qualified Hgc.Lxc as Lxc
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
  doStuff oldname opts = do
    Cvmfs.transaction repository
    capsuleLoc <- copyCapsule oldname newname repositoryLoc
    tmpConfig <- updateConfig newname capsuleLoc (capsuleLoc </> "config")
    Lxc.console newname tmpConfig
    cleanCapsule capsuleLoc
    Cvmfs.publish repository
    where
      repository = optRepository opts
      repositoryLoc = Cvmfs.base </> repository
      newname = capsuleName oldname opts


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
              -> IO FilePath
  copyCapsule oldname newname repobase = cp oldloc newloc >>= \cpStatus ->
    case cpStatus of
      ExitSuccess -> return newloc
      ExitFailure r -> ioError . userError $ "Failure to copy capsule (exit code " ++ show r ++ ")."
    where
      oldloc = repobase </> oldname
      newloc = repobase </> newname
  
  -- Update the config to a temporary location.
  updateConfig :: String -- Capsule name
               -> FilePath -- Capsule location
               -> FilePath -- Config file location
               -> IO FilePath -- Temporary config file location
  updateConfig capsule capsuleLoc configloc = do
    rand <- (randomIO :: IO Int)
    let temploc = "/tmp/config-" ++ show rand
    c <- Lxc.readConfig configloc
    Lxc.writeConfig temploc $ update c
    return temploc
    where update c = Lxc.setConfig "lxc.rootfs" (capsuleLoc </> "rootfs") .
                     Lxc.setConfig "lxc.mount"  (capsuleLoc </> "fstab") .
                     Lxc.setConfig "lxc.utsname" capsule $ c
            
  -- Clean the template, removing specified cache directories (/var/cache/pacman etc)
  cleanCapsule :: FilePath -- Capsule location
               -> IO ()
  cleanCapsule capsule = 
    let cacheDirs = [
            "/var/cache/pacman/pkg"
          ]
    in mapM_ removeDirectoryRecursive $ map (\a -> capsule </> "rootfs" </> a) cacheDirs