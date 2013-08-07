module Hgcdeploy.Config where
  import System.FilePath

  scratch :: FilePath
  scratch = "scratch"

  image :: FilePath
  image = "image"

  runPath :: FilePath
  runPath = "/tmp/hgc"

  autologinFile :: FilePath
  autologinFile = image </> "etc/systemd/system/autologin@.service"

  autologinVar :: String
  autologinVar = "root" -- TODO change this

  passwdFile :: FilePath
  passwdFile = image </> "etc/passwd"

  shadowFile :: FilePath
  shadowFile = image </> "etc/shadow"