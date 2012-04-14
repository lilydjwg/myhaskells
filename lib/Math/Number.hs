module Math.Number (
  nrWidth,
  filesize,
) where

import Text.Printf (printf)

nrWidth :: Integral a => a -> a
nrWidth = ceiling . (logBase 10) . fromIntegral

filesize :: (Integral a, Show a) => a -> String
filesize n = 
  if level /= 0
     then printf "%.1f%ciB" m unit 
     else show n ++ "B"
  where (m, level) = liftUnit (fromIntegral n) 0
        unit = units !! (level-1)

liftUnit :: Double -> Int -> (Double, Int)
liftUnit n u =
  if n > 1100 && u < maxLevel
     then liftUnit (n/1024) (u+1)
     else (n, u)
  where maxLevel = length units

units = "KMGTP"
