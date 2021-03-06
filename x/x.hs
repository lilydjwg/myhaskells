import Control.Applicative ((<$>))
import Control.Monad (mapM_, when)
import Data.List (isSuffixOf, isInfixOf)
import Data.Maybe (isJust, fromJust, isNothing)
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  renameDirectory,
  renameFile,
  getDirectoryContents,
  setCurrentDirectory,
  removeDirectory,
  )
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.FilePath (
  (</>),
  splitFileName,
  dropExtension,
  takeExtension,
  )
import System.Process (createProcess, proc, readProcess, waitForProcess)

import Control.Function (applyUntilM)

main = getArgs >>= mapM_ extract

extract :: FilePath -> IO ()
extract f = do
  let d = snd $ splitFileName $ stripSuffix f
  cmd <- getCmdForFile f
  when (isNothing cmd) $
     putStrLn ("no idea to extract file: " ++ f) >> exitWith (ExitFailure 21)
  createDirectoryIfMissing False d
  setCurrentDirectory d
  exit <- extract' f $ fromJust cmd
  files <- getDirectoryContents "."
  when (length files == 3) $
     moveUpwardsAndDelete d $ head $ filter notRegularDir files
  case exit of
    ExitFailure _ -> exitWith exit
    otherwise -> return ()

notRegularDir "." = False
notRegularDir ".." = False
notRegularDir _ = True

extract' :: FilePath -> [String] -> IO ExitCode
extract' f (cmd:args) =
  waitForProcess =<< fmap fourth (createProcess $ proc cmd (args ++ [".." </> f]))
  where fourth (_, _, _, x) = x

moveUpwardsAndDelete :: FilePath -> FilePath -> IO ()
moveUpwardsAndDelete d f = do
  setCurrentDirectory ".."
  todel <- if d == f
             then do let d' = d ++ tmpSuffix
                     renameDirectory d d'
                     return d'
             else return d
  let f' = todel </> f
  isdir <- doesDirectoryExist f'
  (if isdir then renameDirectory else renameFile) f' f
  removeDirectory todel

tmpSuffix = "._._."

stripSuffix :: FilePath -> String
stripSuffix = stripTar . dropExtension
  where stripTar p = if takeExtension p == ".tar"
                        then dropExtension p
                        else p

getCmdForFile :: FilePath -> IO (Maybe [String])
getCmdForFile = applyUntilM isJust [checkTar, checkRar, checkZipGB, check7z, checkZip]

checkTar, checkRar, checkZip, checkZipGB, check7z :: FilePath -> IO (Maybe [String])

checkTar = return . anySuffix [".tar.gz", ".tar.xz", ".tar.bz2", ".tgz", ".txz", ".tbz", ".tar"] ["tar", "xvf"]
check7z = return . anySuffix [".7z", ".chm"] ["7z", "x"]

checkRar f | ".rar" `isSuffixOf` f = do
  t <- readProcess "file" [f] ""
  return $ Just $ if "Win32" `isInfixOf` t then ["7z", "x"] else ["rar", "x"]
           | otherwise = return Nothing

checkZipGB = return . suffix ".zip" ["gbkunzip"]
checkZip = return . anySuffix [".xpi", ".jar", ".apk", ".maff", ".epub", ".crx", ".whl"] ["unzip"]

suffix :: String -> [String] -> FilePath -> Maybe [String]
suffix suf cmd f = if suf `isSuffixOf` f then Just cmd else Nothing

anySuffix :: [String] -> [String] -> FilePath -> Maybe [String]
anySuffix suffixes cmd f | any (`isSuffixOf` f) suffixes = Just cmd
                         | otherwise = Nothing
