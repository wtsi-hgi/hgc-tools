{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
Rewrite of hgc-deploy in Haskell, aiming to be more configurable and maintainable.

Steps involved:
1. Become root.
2. Create temporary directories.
3. Modify the config and fstab.
4. Establish a union mount for the root filesystem.
5. Modify the contents of the capsule to add the external user as an autologin.
6. Run the capsule in daemon mode.
7. Connect to the capsule using lxc-console.
8. Stop the capsule using lxc-stop.
9. 
-}

module Main where
  import Control.Applicative
  import Control.Concurrent
  import Control.Exception (bracket)
  import Control.Monad.Reader
  import System.Console.GetOpt
  import System.FilePath ((</>))
  import System.Environment (getArgs)
  import System.Log.Logger
  import qualified System.Posix.User as User
  import System.Random (randomIO)

  import qualified Hgc.Cvmfs as Cvmfs
  import qualified Hgc.Lxc as Lxc
  import Hgc.Mount
  import qualified Hgc.Union as Union
  import Hgc.Shell

  -- | Environment
  newtype Env a = Env {
    runE :: ReaderT Options IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Options)

  runEnv :: Env a -> Options -> IO a
  runEnv = runReaderT . runE  

  data Options = Options {
      optRepository :: String
    , optMount :: [FilePath] -- ^ Resources to mount in the capsule.
    , optVerbose :: Bool
    , optUnionType :: Union.Union
    , optScratchPath :: FilePath -- ^ Location on the system to run the capsule in.
  }

  defaultOptions :: Options
  defaultOptions = Options {
      optRepository = "mercury.repo"
    , optMount = []
    , optVerbose = False
    , optUnionType = Union.aufs
    , optScratchPath = "/tmp/hgc"
  }

  setOptions :: [OptDescr (Options -> Options)]
  setOptions =
    [
        Option ['m'] ["mount"] (ReqArg (\n o -> o { optMount = n : optMount o }) "RESOURCE")
          "Load the specified resource into the capsule."
      , Option ['r'] ["repository"] (ReqArg (\n o -> o { optRepository = n }) "REPOSITORY")
          "Use the specified repository name (defaults to mercury.repo)."
      , Option ['v'] ["verbose"] (NoArg (\o -> o  { optVerbose = True }))
          "Enable verbose output."
      , Option ['t'] ["union-type"] (ReqArg (\n o -> setUnionType o n) "UNION_TYPE")
          "Set the type of filesystem used to implement the union mount. Currently supported are aufs and overlayfs."
    ] where setUnionType o "aufs" = o { optUnionType = Union.aufs }
            setUnionType o "overlayfs" = o { optUnionType = Union.overlayfs }
            setUnionType o _ = o

  usage :: String
  usage = usageInfo header setOptions
    where header = "Launch a Mercury capsule.\n" ++
                    "Usage: hgc-deploy [Option...] capsule"

  main :: IO ()
  main = do
    args <- getArgs
    case (getOpt Permute setOptions args) of
      (o,[f],[]) -> runEnv (deploy f) (foldl (flip id) defaultOptions o)
      (_,_,errs) -> putStrLn (concat errs ++ "\n" ++ usage)

  deploy :: String -- ^ Capsule
         -> Env ()
  deploy capsule = ask >>= \options -> do
    liftIO $ when (optVerbose options) $ updateGlobalLogger "hgc" (setLevel DEBUG)
    liftIO $ debugM "hgc" $ "Cloning capsule " ++ capsule
    let sourcePath = Cvmfs.base </> (optRepository options) </> capsule
    (uuid, clonePath) <- cloneCapsule capsule sourcePath
    withRoot $ 
      withUnionMount (sourcePath </> "rootfs") clonePath $ 
      withCapsule uuid (clonePath </> "config") $
      liftIO $ threadDelay 1000000 >> Lxc.console uuid 1
    return ()

  -- | Clone the capsule into a temporary location.
  cloneCapsule :: String -- ^ Name of the capsule template.
               -> FilePath -- ^ Location of the capsule template.
               -> Env (String, FilePath) -- ^ Capsule name, Location on system of the capsule.
  cloneCapsule capsule sourcePath = ask >>= \options -> do
    uuid <- liftIO $ do
      rand <- liftM abs (randomIO :: IO Int)
      un <- User.getLoginName
      return $ un ++ "_" ++ capsule ++ "_" ++ (show rand)
    liftIO $ debugM "hgc" $ "Setting unique capsule ID to " ++ uuid
    let clonePath = (optScratchPath options) </> uuid
    liftIO $ debugM "hgc" $ "Source path: " ++ sourcePath ++ "\nClone path: " ++ clonePath
    liftIO . mkdir $ clonePath
    liftIO $ writeConfig uuid clonePath
    liftIO $ writeFstab clonePath (optMount options)
    return (uuid, clonePath)
    where
      writeConfig uuid clonePath = 
        Lxc.readConfig sourceConf >>= Lxc.writeConfig cloneConf . update
        where 
          update c = Lxc.setConfig "lxc.rootfs" [clonePath </> "image"] .
                     Lxc.setConfig "lxc.mount"  [clonePath </> "fstab"] .
                     Lxc.setConfig "lxc.utsname" [uuid] $ c
          sourceConf = sourcePath </> "config"
          cloneConf = clonePath </> "config"
      writeFstab clonePath mounts' = do
        mounts <- fmap (\a -> fmap (mkFstabEntry . mkBindMount) a) . 
          mapM (\a -> mkMountPoint internalMntDir a) $ mounts'
        readFile (sourcePath </> "fstab") >>= 
          writeFile (clonePath </> "fstab") . writeMounts mounts
        where internalMntDir = clonePath </> "scratch/mnt"
              mkBindMount (e,i) = Mount e i "none" [Bind] []
              writeMounts mounts str = str ++ "\n" ++ unlines mounts

  -- | Perform the given operation with seteuid root.
  withRoot :: Env a -> Env a
  withRoot f = ask >>= \options -> liftIO $ 
    User.getRealUserID >>= \uid -> 
    bracket
      (User.setUserID 0)
      (\_ -> User.setUserID uid)
      (\_ -> runEnv f options)

  -- | Perform the given operation in a union mount.
  withUnionMount :: FilePath -- ^ Lower (ro) dir.
                 -> FilePath -- ^ Clone path.
                 -> Env a -- ^ Operation to perform in union mount.
                 -> Env a -- ^ Result.
  withUnionMount sourcePath clonePath f = ask >>= \options -> do
    let unionfs = optUnionType options
    let union = Mount "none" image (Union.name unionfs) [] [Union.format unionfs sourcePath scratch]
    liftIO $ do
      mkdir $ scratch -- rw dir
      mkdir $ image -- union dir
    liftIO $ bracket
      (mount union)
      (\_ -> umount union)
      (\_ -> runEnv f options)
    where
      scratch =  clonePath </> "scratch"
      image = clonePath </> "image"

  -- | Perform the given operation with a running capsule.
  withCapsule :: String -- ^ Capsule name.
              -> FilePath -- ^ Config file location.
              -> Env a -- ^ Operation to perform with running capsule.
              -> Env a
  withCapsule capsule config f = ask >>= \options ->
    liftIO $ Lxc.withContainerDaemon capsule config (runEnv f options)
