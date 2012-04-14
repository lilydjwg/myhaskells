import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import System.Directory (getDirectoryContents, setCurrentDirectory)
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Monad

isArchDependent :: T.Text -> Bool
isArchDependent = (/= anyarch) . last . (take 2) . (dropWhile (/= archstart)) . T.lines
		  where archstart = T.pack "%ARCH%"
			anyarch = T.pack "any"

filterArchDependent :: [T.Text] -> [T.Text]
filterArchDependent = filter isArchDependent

getPackageName :: T.Text -> T.Text
getPackageName = last . (take 2) . T.lines

topDir = "/var/lib/pacman/local"

getPackagePaths :: IO [FilePath]
getPackagePaths = (filter ((/= '.') . head)) `fmap` getDirectoryContents "."

getPackageDesc :: FilePath -> IO T.Text
getPackageDesc = unsafeInterleaveIO . TIO.readFile . (++ "/desc")

main = do
  setCurrentDirectory topDir
  getPackagePaths >>= mapM getPackageDesc >>= ((mapM TIO.putStrLn) . (map getPackageName) . filterArchDependent)
