module Data.Struct (
  toInt32le,
) where

import Data.Char (ord)

toInt32le :: String -> Int
toInt32le s = sum $ zipWith (*) weights (map ord s)
  where weights = [0x1, 0x100, 0x10000, 0x1000000]
