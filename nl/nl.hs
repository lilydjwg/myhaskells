import Text.Printf (printf)

import Math.Number (nrWidth)
import Text.Lines (getIndent)

addNumber :: String -> String
addNumber s = unlines $ zipWith (numberWithIndent (getIndent ls) (nrWidth $ length ls)) [1..] ls
  where ls = lines s

numberWithIndent :: Int -> Int -> Int -> String -> String
numberWithIndent indent width nr s
  | null s = prefix
  | True   = prefix ++ " " ++ drop indent s
  where fmt = "%" ++ show width ++ "d."
        prefix = replicate indent ' ' ++ printf fmt nr

main = interact addNumber
