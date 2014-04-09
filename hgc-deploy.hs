{-
Copyright (c) 2013, Genome Research Limited
Author: Nicholas Clarke <nicholas.clarke@sanger.ac.uk>

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
9. Unmount the union filesystem.
10. Clean up the temporary directory?
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where
  import Control.Applicative
  import Control.Concurrent
  import Control.Exception (bracket)
  import System.Exit (ExitCode(..))
  import Control.Monad.Reader
  import Data.List (intercalate)
  import System.Console.GetOpt
  import System.Directory (doesDirectoryExist)
  import System.FilePath ((</>))
  import System.Environment (getArgs)
  import System.IO
  import System.Log.Logger
  import qualified System.Posix.Files as Files
  import System.Posix.Types (UserID)
  import qualified System.Posix.User as User
  import System.Random (randomIO)
  import Text.Printf (printf)
  import Text.Regex.TDFA ((=~))

  import qualified Hgcdeploy.Config as Cnf
  import qualified Hgc.Cvmfs as Cvmfs
  import Hgc.Directory
  import qualified Hgc.Lxc as Lxc
  import Hgc.Mount
  import qualified Hgc.Union as Union
  import Hgc.Shell

  -- | Environment
  newtype Env a = Env {
    unEnv :: ReaderT Options IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Options)

  runEnv :: Env a -> Options -> IO a
  runEnv = runReaderT . unEnv  

  data CleanMethod = Chown UserID | Delete

  data Options = Options {
      optRepository :: String
    , optMount :: [FilePath] -- ^ Resources to mount in the capsule.
    , optVerbose :: Bool
    , optUnionType :: Union.Union
    , optRetainTemp :: Bool
  }

  defaultOptions :: Options
  defaultOptions = Options {
      optRepository = "mercury.repo"
    , optMount = []
    , optVerbose = False
    , optUnionType = Union.aufs
    , optRetainTemp = False
  }

  setOptions :: [OptDescr (Options -> Options)]
  setOptions =
    [
        Option ['m'] ["mount"] (ReqArg (\n o -> o { optMount = n : optMount o }) "RESOURCE")
          "Load the specified resource into the capsule."
      , Option ['r'] ["repository"] (ReqArg (\n o -> o { optRepository = n }) "REPOSITORY")
          "Use the specified repository name (defaults to mercury.repo)."
      , Option ['t'] ["union-type"] (ReqArg (\n o -> setUnionType o n) "UNION_TYPE")
          "Set the type of filesystem used to implement the union mount. Currently supported are aufs and overlayfs."
      , Option ['v'] ["verbose"] (NoArg (\o -> o  { optVerbose = True }))
          "Enable verbose output."
      , Option [] ["retain-temp"] (NoArg (\o -> o { optRetainTemp = True }))
          ("Retain the temporary files under "  ++ Cnf.runPath)
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
    liftIO $ debugM "hgc" $ "Deploying capsule " ++ capsule
    liftIO $ do
      debugM "hgc" $ "Setting safe environment."
      setSafeEnv
    liftIO $ do
      unless (capsule =~ validFileRegex) $ ioError . userError $
        "Invalid capsule name: " ++ capsule
      unless ((optRepository options) =~ validFileRegex) $ ioError . userError $
        "Invalid repository name: " ++ (optRepository options)
    realUserID <- liftIO $ User.getRealUserID
    let sourcePath = Cvmfs.base </> (optRepository options) </> capsule
        cleanMethod = if (optRetainTemp options)
          then Chown realUserID
          else Delete
    liftIO $ unlessM (doesDirectoryExist sourcePath) $ ioError . userError $
      "Source path does not exist or is not a directory: " ++ sourcePath
    (uuid, clonePath) <- cloneCapsule capsule sourcePath
    withRoot $ do
      withUnionMount (sourcePath </> "rootfs") clonePath $ do
        addUser realUserID clonePath
        setAutologinUser realUserID clonePath
        withCapsule uuid (clonePath </> "config") $
          liftIO $ threadDelay 1000000 >> Lxc.console uuid 1
      liftIO $ cleanTemp clonePath cleanMethod
    where
      unlessM p m = do
        p' <- p
        unless p' m
      validFileRegex = "^[A-Za-z0-9_][A-Za-z0-9._-]*$"

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
    let clonePath = Cnf.runPath </> uuid
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
          mapM (\a -> mkMountPoint scratchMntDir a) $ mounts'
        readFile (sourcePath </> "fstab") >>= 
          writeFile (clonePath </> "fstab") . writeMounts mounts
        where scratchMntDir = clonePath </> Cnf.scratch </> "mnt"
              imageMntDir = clonePath </> Cnf.image </> "mnt"
              mkBindMount (e,i) = Mount e (imageMntDir </> i) "none" [Bind] []
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
    where image = clonePath </> Cnf.image
          scratch = clonePath </> Cnf.scratch

  {- | Adds the given user into the capsule environment by:
        - Adding entries to the /usr/passwd and /usr/shadow files.
        - Creating a home directory.
  -}
  addUser :: UserID
          -> FilePath -- ^ Clone path.
          -> Env ()
  addUser uid clonePath = do
    liftIO $ debugM "hgc" $ "Adding user with ID " ++ (show uid) ++ " into container."
    ue <- liftIO $ User.getUserEntryForID uid
    let username = User.userName ue
        intHomedir = "/home" </> username
        extHomedir = clonePath </> "image" ++ intHomedir -- intHomeDir is absolute, so </> fails
        newue = ue { User.homeDirectory = intHomedir }
    liftIO $ do
      debugM "hgc" $ printf "Username: %s\nHomedir: %s" username intHomedir
      mkdir extHomedir
      Files.setOwnerAndGroup extHomedir uid (-1)
      withFile (clonePath </> Cnf.passwdFile) AppendMode (\h ->
        let pwentry = mkPasswdEntry newue in 
          debugM "hgc" ("Adding passwd entry: " ++ pwentry) >>
          hPutStrLn h pwentry
        )
      withFile (clonePath </> Cnf.shadowFile) AppendMode (\h ->
        let pwentry = mkShadowEntry newue in 
          debugM "hgc" ("Adding shadow entry: " ++ pwentry) >>
          hPutStrLn h pwentry
        )
    where mkPasswdEntry (User.UserEntry n p i g ge h s) =
            intercalate ":" [n,p,show i,show g,ge,h,s] 
          mkShadowEntry (User.UserEntry n _ _ _ _ _ _) =
            n ++ ":*:::::::"

  -- | Set the autologin user.
  setAutologinUser :: UserID
                   -> FilePath -- ^ Clone path.
                   -> Env ()
  setAutologinUser uid clonePath = do
    ue <- liftIO $ User.getUserEntryForID uid
    let username = User.userName ue
    liftIO $ safeSystem "sed" [
        "-i"
      , "s/" ++ Cnf.autologinVar ++ "/" ++ username ++ "/"
      , clonePath </> Cnf.autologinFile
      ] >>= \ex -> case ex of
        ExitSuccess -> return ()
        ExitFailure r -> ioError . userError $ 
          "Cannot set autologin user (exit code " ++ show r ++ ")."

  -- | Perform the given operation with a running capsule.
  withCapsule :: String -- ^ Capsule name.
              -> FilePath -- ^ Config file location.
              -> Env a -- ^ Operation to perform with running capsule.
              -> Env a
  withCapsule capsule config f = ask >>= \options ->
    liftIO $ Lxc.withContainerDaemon capsule config "/sbin/init" (runEnv f options)

  cleanTemp :: FilePath -- ^ Clone location
            -> CleanMethod -- ^ Whether to delete or chown the temp dir
            -> IO ()
  cleanTemp clonePath Delete = do
    debugM "hgc" $ "Removing directory " ++ clonePath
    removeDirectoryRecursive NoFollow clonePath
  cleanTemp clonePath (Chown uid) = do
    debugM "hgc" $ "Changing ownership on directory " ++ clonePath
    chownRecursive NoFollow uid (-1) clonePath
