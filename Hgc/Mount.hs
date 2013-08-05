module Hgc.Mount (
    mkMountPoint
  , mkFstabEntry
  , mount
  , SLM.umount
  , SLM.MountFlag(..)
  , Mount (..)
  )
where
  import System.Log.Logger
  import System.Directory (canonicalizePath
    , doesFileExist
    , doesDirectoryExist)
  import System.FilePath
  import qualified System.Linux.Mount as SLM
  
  import Data.List (intercalate)
  import Data.Maybe (catMaybes)
  import qualified Data.ByteString.Char8 as B (pack)

  import Hgc.Shell

  data Mount = Mount 
    String -- ^ Mount from
    FilePath -- ^ Mount to
    String -- ^ Mount type
    [SLM.MountFlag] -- ^ System independent mount options
    [String] -- ^ System dependent mount options

  -- | Convert SML option to a string suitable for use in 
  slmOptionString :: SLM.MountFlag -> Maybe String
  slmOptionString opt = case opt of
    SLM.Rdonly -> Just "ro"
    SLM.Remount -> Just "remount"
    SLM.Noatime -> Just "noatime"
    SLM.Bind -> Just "bind" 
    _ -> Nothing

  -- | Mount a filesystem.
  mount :: Mount -> IO() 
  mount (Mount from to typ opt1 opt2) =
    mkdir to >>
    SLM.mount from to typ opt1 dd
    where dd = B.pack . intercalate "," $ opt2

  mkFstabEntry :: Mount -> String
  mkFstabEntry (Mount from to typ opt1 opt2) =
    intercalate " " [from, to, typ, intercalate "," opts, "0", "0"]
    where opts = (catMaybes $ map slmOptionString opt1) ++ opt2

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