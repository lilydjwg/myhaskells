import Control.Applicative ((<*>), (<$>))
import Control.Monad.Instances
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith)
import System.IO (hGetContents, hSetBuffering, BufferMode(LineBuffering))
import System.Process (
  proc,
  createProcess,
  waitForProcess,
  CmdSpec(RawCommand),
  CreateProcess(..),
  ProcessHandle,
  StdStream(..),
  )

import Control.Function (applyEither)
import Text.String (dropPrefix)

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
  (_, Just out, _, p) <- createProcess (proc "locate" args''){ std_in = Inherit,
                                                               std_out = CreatePipe,
                                                               std_err = Inherit }
  hSetBuffering out LineBuffering
  (,) <$> hGetContents out <*> return p

transform :: String -> String
transform = unlines . map transformLine . lines

transformLine :: String -> String
transformLine s = case applyEither funcs s of
                     Left r -> r
                     Right r -> '~' : r
  where funcs = map dropPrefix prefixesToHome

prefixesToHome :: [String]
prefixesToHome = ["/home/.ecryptfs/lilydjwg/public", "/home/lilydjwg"]
