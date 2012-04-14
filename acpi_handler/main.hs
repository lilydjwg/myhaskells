module Main where

import Control.Monad (when)
import List (isPrefixOf)
import System.Cmd (rawSystem)
import System.Environment (getArgs)
import System.Exit
import System.Posix.Env (putEnv)
import System.Process (readProcessWithExitCode)

import Text.String (isSpace, stripl)

main :: IO ()
main = do
  args <- getArgs
  when (length args < 1) exitFailure
  let funcargs = tail args
  case head args of
    "button/zoom" -> touchpadSwitch funcargs
    _ -> logger $ "ACPI group/action undefined: " ++ show args

logger :: String -> IO ()
logger = putStrLn

data TouchpadState = On | Off deriving (Eq, Show)

touchpadSwitch :: [String] -> IO ()
touchpadSwitch args = do
  setupDisplay
  mbstate <- touchpadGetstate
  case mbstate of
    Nothing -> return ()
    Just status -> touchpadSetstate $ touchpadNegate status

touchpadGetstate :: IO (Maybe TouchpadState)
touchpadGetstate = do
  (status, info, err) <- readProcessWithExitCode "sudo" ["-u", "lilydjwg", "synclient"] ""
  if status /= ExitSuccess
     then return Nothing
     else return $ touchpadGetstateFromString info

touchpadSetstate :: TouchpadState -> IO ()
touchpadSetstate state = rawSystem "sudo" ["-u", "lilydjwg", "synclient",
  "TouchpadOff=" ++ touchpadToOutside state] >> return ()

touchpadGetstateFromString :: String -> Maybe TouchpadState
touchpadGetstateFromString s =
  if length line == 1
     then case last $ head line of
       '0' -> Just On
       '1' -> Just Off
       otherwise -> Nothing
     else Nothing
  where line = filter (isPrefixOf "TouchpadOff") $ map stripl $ lines s

touchpadNegate :: TouchpadState -> TouchpadState
touchpadNegate On = Off
touchpadNegate Off = On

touchpadToOutside :: TouchpadState -> String
touchpadToOutside On = "0"
touchpadToOutside Off = "1"

setupDisplay :: IO ()
setupDisplay = putEnv $ "DISPLAY=:0"

