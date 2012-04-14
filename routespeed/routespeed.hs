import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Data.List
import Data.List.Split (endBy)
import Data.Maybe
import Data.Time (formatTime, getZonedTime)
import Network.Browser
import Network.HTTP
import Network.URI (parseURI)
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)

import Text.String (parseInt)
import Math.Number (filesize)

url = "http://192.168.1.1/userRpm/StatusRpm.htm"
auth = AuthBasic "" "admin" "admin" (fromJust $ parseURI "http://192.168.1.1/")

main = httpreq >>= loop
  where loop a = printSpeed a >>= loop

printSpeed :: [Integer] -> IO [Integer]
printSpeed a = do
  threadDelay $ 1 * 1000 * 1000
  (b, c) <- getSpeed a
  printCurrentTime
  putStrLn . concat . intersperse ", " . map ((++ "/s") . printf "%8s" . filesize) $ c
  return b

getSpeed :: [Integer] -> IO ([Integer], [Integer])
getSpeed a = do
  b <- httpreq
  let c = zipWith (-) b a
  return (b, c)

httpreq :: IO [Integer]
httpreq = do 
  rsp <- browse $ do
    setAllowRedirects True
    addAuthority auth
    setOutHandler $ const $ return ()
    request $ getRequest url
  return $ getBytes $ rspBody $ snd rsp

getBytes :: String -> [Integer]
getBytes = take 2 . map (fromJust . parseInt) . endBy ", " . getDataLine

getDataLine :: String -> String
getDataLine = (!!1) . (dropWhile (not . isPrefixOf "var statistList")) . lines

printCurrentTime :: IO ()
printCurrentTime = curTime >>= putStr >> putChar ' '

curTime :: IO String
curTime = formatTime defaultTimeLocale "%H:%M:%S" <$> getZonedTime
