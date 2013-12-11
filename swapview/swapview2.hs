{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables #-}

-- Thanks to przhu
-- https://gist.github.com/przhu/7892814

import Control.Applicative ((<$>))
import Control.Exception (catch, SomeException)
import Control.Monad (mapM)
import Data.Char (isDigit)
import Data.List (sortBy)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Read(decimal)
import System.Directory (getDirectoryContents)
import Text.Printf (printf)
import Control.Arrow(second)

import Math.Number (filesize)
-- filesize :: Int -> String
-- filesize = show

type Pid = T.Text

format :: String
format = "%5s %9s %s"
totalFmt :: String
totalFmt = "Total: %8s"
pidTitl :: String
pidTitl = "PID"
swapTitl :: String
swapTitl = "SWAP"
cmdTitl :: String
cmdTitl = "COMMAND"

main = do
  ps <- pids
  ss <- mapM swapusedNoExcept ps
  let !t = 1024 * sum ss
  r <- mapM formatResult (transformData (zip ps ss))
  let printResult = do
        putStrLn $ printf format pidTitl swapTitl cmdTitl
        putStr . unlines $ r
        putStrLn $ printf totalFmt $ filesize t
  printResult
    where swapusedNoExcept !p = do
              su <- catch (swapused p) (\(_::SomeException) -> return 0)
              return $! su

pids :: IO [Pid]
pids = filter digitsOnly . map T.pack <$> getDirectoryContents "/proc"
  where digitsOnly = T.all isDigit

swapused :: Pid -> IO Int
swapused pid = sum . map getNumber . filter (T.isPrefixOf "Swap:") . T.lines <$>
                 T.readFile (T.unpack $ "/proc/" `T.append` pid `T.append` "/smaps")
  where getNumber line =
          case T.dropWhile (not.isDigit) line of
            t -> case decimal t of
                   (Right (n, _)) -> n
                   (Left _) -> 0

transformData :: [(Pid, Int)] -> [(Pid, String)]
transformData = map (second humanSize) . 
                sortBy (\ (_, !x) (_, !y) -> compare x y) .
                filter ((/=) 0 . snd)
  where humanSize = filesize . (* 1024)

formatResult :: (Pid, String) -> IO String
formatResult (pid, size) = do
  cmd <- getCommand pid
  return $ printf format (T.unpack pid) size (T.unpack cmd)

getCommand :: Pid -> IO T.Text
getCommand pid = T.init <$> T.map transnul <$> T.readFile (T.unpack $ "/proc/" `T.append` pid `T.append` "/cmdline")
  where transnul ch = if ch == '\0' then ' ' else ch

