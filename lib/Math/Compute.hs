module Math.Compute (
  primes,
  isPrime,
  factorial,
) where

import Control.Monad
import Control.Monad.Instances

isPrime :: Integral a => a -> Bool
primes :: Integral a => [a]
isPrime x = let prime_useful = takeWhile (\y -> y*y <= x) primes
            in all (\y -> x `mod` y /= 0) prime_useful
primes = 2 : filter isPrime [3, 5..]

factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = n * (factorial (n-1))
