module Hgc.Directory where
  import System.Directory (
      getDirectoryContents
    , removeDirectory
    , removeFile
    )
  import System.FilePath ((</>))
  import System.Posix.Files
  import System.Posix.Types (UserID, GroupID)

  data FollowSymlinks = Follow | NoFollow

  removeDirectoryRecursive :: FollowSymlinks
                           -> FilePath
                           -> IO ()
  removeDirectoryRecursive fs fp = do
    cont <- getDirectoryContents fp
    sequence_ [rm (fp </> x) | x <- cont, x /= "." && x /= ".."]
    removeDirectory fp
    where
      rm :: FilePath -> IO ()
      rm f = do
        stat <- getStatus f
        case (isDirectory stat, isSymbolicLink stat) of
          (True, _) -> removeDirectoryRecursive fs f
          (_, True) -> removeLink f
          _ -> removeFile f
      getStatus = case fs of
        Follow -> getFileStatus
        NoFollow -> getSymbolicLinkStatus

  chownRecursive :: FollowSymlinks
                 -> UserID
                 -> GroupID -- if set to -1, do not change
                 -> FilePath
                 -> IO ()
  chownRecursive fs uid gid fp =
    chownRecursive' fp where 
      chownRecursive' f = do
        cont <- getDirectoryContents f
        sequence_ [chown (f </> x) | x <- cont, x /= "." && x /= ".."]
        setOwner f
      chown f = do
        stat <- getStatus f
        if (isDirectory stat) then chownRecursive' f else setOwner f
      getStatus = case fs of
        Follow -> getFileStatus
        NoFollow -> getSymbolicLinkStatus
      setOwner f = case fs of
        Follow -> setOwnerAndGroup f uid gid
        NoFollow -> setSymbolicLinkOwnerAndGroup f uid gid