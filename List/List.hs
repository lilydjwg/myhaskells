module List.List (
  lookupWith,
) where

lookupWith :: (a -> Bool) -> [(a, b)] -> Maybe b
lookupWith _ []          = Nothing
lookupWith p ((x, y):xs) | p x = Just y
                         | otherwise = lookupWith p xs
