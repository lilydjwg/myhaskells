-- TODO: be lazy
--       handle failed (nothing found)
--       use argv0 to determine which to use

import Control.Applicative ((<*>), (<$>))
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith)
import System.IO (hGetContents)
import System.Process (
  proc,
  createProcess,
  waitForProcess,
  CmdSpec(RawCommand),
  CreateProcess(..),
  ProcessHandle,
  StdStream(..),
  )

main = do
  (out, p) <- doLocate
  putStr $ transform out
  waitForProcess p >>= exitWith

doLocate :: IO (String, ProcessHandle)
doLocate = do
  argv0 <- getProgName
  let args = case argv0 of
                  "lre" -> ["-b", "--regex"]
                  _ -> []
  args' <- getArgs
  let args'' = args ++ args'
  (_, out, _, p) <- createProcess (proc "locate" args''){ std_in = Inherit,
                                                          std_out = CreatePipe,
                                                          std_err = Inherit }
  (,) <$> (hGetContents . fromJust) out <*> return p

transform :: String -> String
transform = unlines . map transformLine . lines

transformLine :: String -> String
transformLine line
 | ecryptfsPub `isPrefixOf` line = '~' : drop (length ecryptfsPub) line
 | myhome `isPrefixOf` line = '~' : drop (length myhome) line
 | otherwise = line

ecryptfsPub = "/home/.ecryptfs/lilydjwg/public" 
myhome = "/home/lilydjwg"
