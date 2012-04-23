module Control.Function (
  applyMaybe,
  applyUntil,
  applyUntilM,
) where

import Data.Maybe (isJust)

-- |Applies a list of functions until a Just value is got
applyMaybe :: [(a -> Maybe b)] -> a -> Maybe b
applyMaybe = applyUntil isJust

-- |applies until p becomes True
applyUntil :: (b -> Bool) -> [(a -> b)] -> a -> b
applyUntil _ (f:[]) a = f a
applyUntil p (f:fs) a = if p r then r else applyUntil p fs a
                           where r = f a

applyUntilM :: Monad m => (b -> Bool) -> [(a -> m b)] -> a -> m b
applyUntilM _ (f:[]) a = f a
applyUntilM p (f:fs) a = do 
  r <- f a
  if p r then return r else applyUntilM p fs a

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True
