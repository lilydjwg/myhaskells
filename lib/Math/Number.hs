module Math.Number (
  nrWidth,
  filesize,
) where

import Text.Printf (printf)

nrWidth :: Integral a => a -> a
nrWidth = ceiling . logBase 10 . fromIntegral

filesize :: (Integral a, Show a) => a -> String
filesize n =
  if not.null $ level
     then printf "%.1f%ciB" m unit
     else show n ++ "B"
  where (m, level) = liftUnit (fromIntegral n) units []
        unit = head level

liftUnit :: Double -> [Char] -> [Char] -> (Double, [Char])
liftUnit n u l =
  if n > 1100 && (not.null) u
     then liftUnit (n/1024) (tail u) (head u :l)
     else (n, l)

units = "KMGTP"
