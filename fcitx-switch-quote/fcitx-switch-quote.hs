import Control.Applicative ((<$>))
import System.Cmd (rawSystem)
import System.Directory (getHomeDirectory)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.String (trChar)

main = do
  file <- getFile
  TIO.readFile file >>= (TIO.writeFile file) . (T.map (trChar "“”‘’『』「」" "『』「」“”‘’"))
  reloadFcitx

getFile :: IO FilePath
getFile = (++ "/.config/fcitx/data/punc.mb.zh_CN") <$> getHomeDirectory

reloadFcitx :: IO ()
reloadFcitx = rawSystem "fcitx-remote" ["fcitx-remote", "-r"] >> return ()
