module Sys.Path (
  expandUser,
) where

import Control.Applicative ((<$>))
import System.Directory (getHomeDirectory)
import System.Posix.User (homeDirectory, getUserEntryForName, UserEntry(..))

expandUser :: FilePath -> IO FilePath
expandUser "~"         = getHomeDirectory
expandUser ('~':'/':p) = fmap (++ "/" ++ p) getHomeDirectory
expandUser ('~':up)    = let (u, p) = break (== '/') up
                             in fmap (++ tail p) (homeDirectory <$> getUserEntryForName u)
expandUser p           = return p
