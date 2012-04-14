import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getDirectoryContents, setCurrentDirectory)
import System.FilePath ((</>))
import System.Process (readProcess)
import Control.Monad
import Control.Applicative ((<$>))

isArchDependent :: T.Text -> Bool
isArchDependent = (/= anyarch) . last . (take 2) . (dropWhile (/= archstart)) . T.lines
		  where archstart = T.pack "%ARCH%"
			anyarch = T.pack "any"

filterArchDependent :: [T.Text] -> [T.Text]
filterArchDependent = filter isArchDependent

inPacman :: [T.Text] -> T.Text -> Bool
inPacman pkgs x = x `elem` pkgs

getPackageName :: T.Text -> T.Text
getPackageName = last . (take 2) . T.lines

topDir = "/var/lib/pacman/local"

getPackagePaths :: IO [FilePath]
getPackagePaths = liftM (map (topDir </>)) $ (filter ((/= '.') . head)) <$> getDirectoryContents topDir

getPackageDesc :: FilePath -> IO T.Text
getPackageDesc = TIO.readFile . (++ "/desc")

allPacmanPackages :: IO [T.Text]
allPacmanPackages = readProcess "pacman" ["-Sl"] "" >>= return . map ((!! 1) . T.split (==' ')) . T.lines . T.pack

main = do
  pkgs <- allPacmanPackages
  getPackagePaths >>= mapM getPackageDesc >>= (mapM_ TIO.putStrLn) . (filter $ inPacman pkgs) . (map getPackageName) . (filter isArchDependent)
