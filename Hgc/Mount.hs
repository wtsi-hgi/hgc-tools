module Hgc.Mount 
  (mkMountPoint)
where
  import System.Log.Logger
  import System.Directory (canonicalizePath
    , doesFileExist
    , doesDirectoryExist)
  import System.FilePath
  import Hgc.Shell

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
      (False, True) -> debugM "hgc.mount" ("Touching file mountpoint " ++ resourceC) >>
        touch mp >> return (resourceC, mp)
      (True, False) -> debugM "hgc.mount" ("Making directory mountpoint " ++ resourceC) >>
        mkdir mp >> return (resourceC, mp)
      (True, True) -> ioError . userError $ 
        "Really weird: mount point is both file and directory: " ++ resource
      (False, False) -> ioError . userError $ 
        "Mount point does not exist or cannot be read: " ++ resource