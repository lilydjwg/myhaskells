import Control.Applicative ((<*>), (<$>))
import Control.Monad.Instances
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith)
import System.IO (hGetContents, hSetBuffering, BufferMode(LineBuffering))
import System.Posix.Terminal (queryTerminal)
import System.Process (
  proc,
  createProcess,
  waitForProcess,
  CmdSpec(RawCommand),
  CreateProcess(..),
  ProcessHandle,
  StdStream(..),
  )

import List.List (lookupWith)

main = do
  (out, p) <- doLocate
  isatty <- queryTerminal 1
  putStr $ (if isatty then transform else id) out
  waitForProcess p >>= exitWith

doLocate :: IO (String, ProcessHandle)
doLocate = do
  argv0 <- getProgName
  let args = case argv0 of
                  "lre" -> ["-b", "--regex"]
                  "lrew" -> ["--regex"]
                  "l" -> []
                  _ -> error $ "I'm not " ++ argv0 ++ "."
  args' <- getArgs
  let args'' = args ++ args'
  (_, Just out, _, p) <- createProcess (proc "locate" args''){ std_in = Inherit,
                                                               std_out = CreatePipe,
                                                               std_err = Inherit }
  hSetBuffering out LineBuffering
  (,) <$> hGetContents out <*> return p

transform :: String -> String
transform = unlines . map transformLine . lines

transformLine :: String -> String
transformLine s = case lookupWith (`isPrefixOf` s) prefixesMap of
  Just (p, r) -> r ++ drop (length p) s
  Nothing -> s

prefixesMap :: [(String, String)]
prefixesMap = [
  ("/home/lilydjwg", "~"),
  ("/home/.ecryptfs/lilydjwg/public/aMule", "~/.aMule"),
  ("/home/.ecryptfs/lilydjwg/public/goldendict", "~/.goldendict/goldendict"),
  ("/home/.ecryptfs/lilydjwg/public/VirtualBox", "~/.VirtualBox"),
  ("/home/.ecryptfs/lilydjwg/public", "~")
  ]
