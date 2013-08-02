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
  import Prelude hiding (mapM)
  import Control.Monad (when, unless, filterM, liftM, join)
  import Data.Maybe (maybeToList)
  import Data.List (intercalate)
  import Data.List.Split (splitOneOf)
  import Data.Traversable (mapM)
  import System.Console.GetOpt
  import System.Environment (getArgs)
  import System.Exit (ExitCode(..))
  import System.FilePath
  import System.Random (randomIO)
  import System.Directory (canonicalizePath
    , doesFileExist
    , doesDirectoryExist
    , removeDirectoryRecursive)
  import System.Log.Logger

  import qualified Hgc.Cvmfs as Cvmfs
  import qualified Hgc.Lxc as Lxc
  import Hgc.Shell

  data Options = Options {
      optPublish :: Bool -- ^ Automatically publish the CVMFS repo
    , optMajorRevision :: Bool -- ^ Create a new major revision
    , optNewCapsule :: Maybe String -- ^ Name for new capsule
    , optRepository :: String
    , optCloneOnly :: Bool -- ^ Only clone the template, don't start as a capsule.
    , optMount :: [FilePath] -- ^ Resources to mount in the capsule.
    , optPkgSrcDir :: Maybe FilePath -- ^ Location to use for package source cache.
    , optVerbose :: Bool
  }

  defaultOptions :: Options
  defaultOptions = Options {
      optPublish = False
    , optMajorRevision = False
    , optNewCapsule = Nothing
    , optRepository = "mercury.repo"
    , optCloneOnly = False
    , optMount = []
    , optPkgSrcDir = Nothing
    , optVerbose = False
  }

  options :: [OptDescr (Options -> Options)]
  options =
    [
        Option ['y'] ["publish"] (NoArg (\o -> o { optPublish = True })) 
          "Automatically publish new capsule."
      , Option ['M'] ["major"] (NoArg (\o -> o { optMajorRevision = True })) 
          "Create a major revision."
      , Option ['n'] ["new-capsule"] (ReqArg (\n o -> o { optNewCapsule = Just n }) "NAME") 
          "Create a capsule under a new name."
      , Option ['r'] ["repository"] (ReqArg (\n o -> o { optRepository = n }) "REPOSITORY")
          "Use the specified repository name (defaults to mercury.repo)."
      , Option [] ["clone-only"] (NoArg (\o -> o { optCloneOnly = True }))
          "Only clone the template, don't start the capsule."
      , Option ['m'] ["mount"] (ReqArg (\n o -> o { optMount = n : optMount o }) "MOUNT_POINT")
          "Mount the specified resource into the capsule."
      , Option ['p'] ["pkgdir"] (ReqArg (\n o -> o { optPkgSrcDir = Just n}) "PKGDIR")
          "Use specified directory in place of the aura source package cache."
      , Option ['v'] ["verbose"] (NoArg (\o -> o  { optVerbose = True }))
          "Enable verbose logging."
    ]

  usage :: String
  usage = usageInfo header options
    where header = "Create a new version of a Mercury capsule based on the specified template.\n" ++
                    "Usage: hgc-version [Option...] template"

  main :: IO ()
  main = do
    args <- getArgs
    case (getOpt Permute options args) of
      (o,[f],[]) -> doStuff f (foldl (flip id) defaultOptions o)
      (_,_,errs) -> putStrLn (concat errs ++ "\n" ++ usage)

  doStuff :: String
          -> Options
          -> IO ()
  doStuff oldname opts = do
    when (optVerbose opts) $ updateGlobalLogger "hgc-version" (setLevel DEBUG)
    Cvmfs.inTransaction repository publish $ do
      capsuleLoc <- copyCapsule oldname newname repositoryLoc
      debugM "hgc-version" $ "New capsule location: " ++ capsuleLoc
      tmpConfig <- updateConfig newname capsuleLoc opts
      debugM "hgc-version" $ "Config location: " ++ tmpConfig
      unless (optCloneOnly opts) $ Lxc.console newname tmpConfig
      cleanCapsule capsuleLoc
    where
      repository = optRepository opts
      publish = optPublish opts
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
  copyCapsule oldname newname repobase =
    debugM "hgc-version" ("Copying " ++ oldname ++ " to " ++ newname ++ " in repository " ++ repobase) >>
    cp oldloc newloc >>= \cpStatus ->
    case cpStatus of
      ExitSuccess -> return newloc
      ExitFailure r -> ioError . userError $ "Failed to copy capsule (exit code " ++ show r ++ ")."
    where
      oldloc = repobase </> oldname
      newloc = repobase </> newname
  
  -- Update the config to a temporary location.
  updateConfig :: String -- Capsule name
               -> FilePath -- Capsule location
               -> Options -- Options
               -> IO FilePath -- Temporary config file location
  updateConfig capsule capsuleLoc opts = do
    rand <- (randomIO :: IO Int)
    let tmpConfig = "/tmp/config-" ++ capsule ++ show rand
    let tmpFstab = "/tmp/fstab-" ++ capsule ++ show rand
    let update c =  Lxc.setConfig "lxc.rootfs" [(capsuleLoc </> "rootfs")] .
                    Lxc.setConfig "lxc.mount"  [tmpFstab] .
                    Lxc.setConfig "lxc.utsname" [capsule] $ c
    writeFstab capsuleLoc tmpFstab opts
    Lxc.readConfig configloc >>= Lxc.writeConfig tmpConfig . update
    return tmpConfig
    where 
      configloc = (capsuleLoc </> "config")
            
  -- Write the FSTAB for the capsule.
  writeFstab :: FilePath -- ^ Capsule location.
             -> FilePath -- ^ New fstab location
             -> Options
             -> IO ()
  writeFstab capsuleLoc tmpFstab opts = do
    pkgSrcMnt <- fmap (\a -> fmap (mkBindMount internalPkgSrcDir) a) 
        . mapM canonicalizePath $ optPkgSrcDir opts
    otherMounts <- fmap (\a -> fmap mkMount a)
        . mapM (\a -> mkMountPoint internalMntDir a) 
        $ optMount opts
    let mounts = (maybeToList pkgSrcMnt) ++ otherMounts
    mapM_ (debugM "hgc-version" . (\a -> "Mount point: " ++ a)) mounts
    let writeMounts str = str ++ "\n" ++ unlines mounts
    readFile fstabloc >>= writeFile tmpFstab . writeMounts
    where
      fstabloc = (capsuleLoc </> "fstab")
      internalPkgSrcDir = capsuleLoc </> "rootfs/var/cache/aura/src"
      internalMntDir = capsuleLoc </> "rootfs/mnt"
      mkBindMount int ext = intercalate " " [ext, int, "none", "bind", "0", "0"]
      mkMount (a,b) = mkBindMount b a

  -- Make a mount point in the container to mount on top of
  mkMountPoint :: FilePath -- ^ Root mount point
               -> FilePath -- ^ Thing to mount
               -> IO (FilePath, FilePath) -- ^ resource, Created mountpoint
  mkMountPoint mountLoc resource = do
    resourceC <- canonicalizePath . dropTrailingPathSeparator $ resource
    isDir <- doesDirectoryExist resourceC
    isFile <- doesFileExist resourceC
    let mp = mountLoc </> resource
    mkdir $ mountLoc </> (dropFileName resourceC)
    case (isDir, isFile) of
      (False, True) -> debugM "hgc-version" ("Touching file mountpoint " ++ resourceC) >>
        touch mp >> return (resourceC, mp)
      (True, False) -> debugM "hgc-version" ("Making directory mountpoint " ++ resourceC) >>
        mkdir mp >> return (resourceC, mp)
      (True, True) -> ioError . userError $ 
        "Really weird: mount point is both file and directory: " ++ resource
      (False, False) -> ioError . userError $ 
        "Mount point does not exist or cannot be read: " ++ resource

  -- Clean the template, removing specified cache directories (/var/cache/pacman etc)
  cleanCapsule :: FilePath -- Capsule location
               -> IO ()
  cleanCapsule capsule = 
    let cacheDirs = [
              "var/cache/pacman/pkg"
            , "var/log"
          ]
        rmdir dir = do
          debugM "hgc-version" $ "Removing directory " ++ dir
          removeDirectoryRecursive dir
        isDir dir = do
          exists <- doesDirectoryExist dir
          unless (exists) $ debugM "hgc-version" $ "Directory " ++ dir ++ " does not exist."
          return exists
    in do 
      debugM "hgc-version" ("Cleaning capsule " ++ capsule)
      join . (liftM $ mapM_ rmdir) . filterM isDir $ 
        map (\a -> capsule </> "rootfs" </> a) cacheDirs