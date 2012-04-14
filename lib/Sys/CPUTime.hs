module Sys.CPUTime (
  timeit,
) where

import CPUTime (getCPUTime)

timeit :: IO () -> IO Integer
timeit action = do
  a <- getCPUTime
  action
  b <- getCPUTime
  return $ b - a
