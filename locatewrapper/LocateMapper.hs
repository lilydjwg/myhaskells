module LocateMapper (
  readLocate,
  transform,
) where

import Data.List (isPrefixOf)
import System.Process (readProcess)
import System.Environment (getArgs)

readLocate :: [String] -> IO String
readLocate args = getArgs >>= \cmd ->
  let args' = args ++ cmd
  in readProcess "locate" args' ""


transform :: String -> String
transform = unlines . map transformLine . lines

transformLine :: String -> String
transformLine line
 | ecryptfsPub `isPrefixOf` line = '~' : drop (length ecryptfsPub) line
 | myhome `isPrefixOf` line = '~' : drop (length myhome) line
 | otherwise = line

ecryptfsPub = "/home/.ecryptfs/lilydjwg/public" 
myhome = "/home/lilydjwg"
