import System.Environment
import System.IO
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List

main = do
  diamond >>= mapM BS.putStr

diamond :: IO ([BS.ByteString])
diamond = do
  args <- getArgs
  let filenames = fst $ getFileArgs args
  if null filenames then
    BS.getContents >>= return . (:[])
  else
    forM filenames (\f -> case f of
      "-" -> BS.getContents
      otherwise -> BS.readFile f)

isFileName :: String -> Bool
isFileName "-" = True
isFileName ('-':_) = False
isFileName _ = True

getFileArgs :: [String] -> ([String], [String])
getFileArgs = partition isFileName
