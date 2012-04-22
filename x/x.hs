import Control.Applicative ((<$>))
import Control.Monad (mapM_)
import Data.List (isSuffixOf, isInfixOf)
import Data.Maybe (isJust, fromJust)
import System.Cmd (rawSystem)
import System.Directory (
  createDirectoryIfMissing,
  renameDirectory,
  getDirectoryContents,
  setCurrentDirectory,
  removeDirectory,
  )
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.FilePath ((</>))
import System.Process (readProcess)

import Control.Function (applyUntilM)

main = getArgs >>= mapM_ extract

extract :: FilePath -> IO ()
extract f = do
  let d = stripSuffix f
  createDirectoryIfMissing False d
  setCurrentDirectory d
  exit <- extract' $ ".." </> f
  files <- getDirectoryContents "."
  if length files == 3
     then moveUpwardsAndDelete d $ last files
     else return ()
  case exit of
    ExitFailure _ -> exitWith exit
    otherwise -> return ()

extract' :: FilePath -> IO ExitCode
extract' f = do
  cmd <- getCmdForFile f
  if isJust cmd
     then let cmd':args = fromJust cmd in rawSystem cmd' (args ++ [f])
     else exitWith $ ExitFailure $ negate 1

moveUpwardsAndDelete :: FilePath -> FilePath -> IO ()
moveUpwardsAndDelete d f = do
  print "deleting"
  print (d, f)
  setCurrentDirectory ".."
  del' <- if d == f
             then do let d' = d ++ tmpSuffix
                         f' = d' ++ "/" ++ f
                     print (d, d')
                     renameDirectory d d'
                     print (f', f)
                     renameDirectory f' f
                     return d'
             else return d
  removeDirectory del'

tmpSuffix = "._._."

stripSuffix :: FilePath -> String
stripSuffix = stripTar . reverse . tail . dropWhile (/='.') . reverse
  where stripTar p = if ".tar" `isSuffixOf` p
                        then take (length p - 4) p
                        else p

getCmdForFile :: FilePath -> IO (Maybe [String])
getCmdForFile = applyUntilM isJust [checkTar, checkRar, checkZip, check7z]

checkTar, checkRar, checkZip, check7z :: FilePath -> IO (Maybe [String])
checkTar f | any (`isSuffixOf` f) [".tar.gz", ".tar.xz", ".tar.bz2",
                                   ".tgz", ".txz", ".tbz"]
                                     = return $ Just ["tar", "xvf"]
           | otherwise = return Nothing

checkRar f | ".rar" `isSuffixOf` f = do
  t <- readProcess "file" [f] ""
  return $ Just $ if "Win32" `isInfixOf` t then ["7z", "x"] else ["rar", "x"]
           | otherwise = return Nothing

checkZip = return . suffix ".zip" ["gbkunzip"]
check7z = return . suffix ".7z" ["7z", "x"]

suffix :: String -> [String] -> FilePath -> Maybe [String]
suffix suf cmd f = if suf `isSuffixOf` f then Just cmd else Nothing
