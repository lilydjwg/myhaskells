module List.List (
  lookupWith,
) where

lookupWith :: (a -> Bool) -> [(a, b)] -> Maybe (a, b)
lookupWith _ []          = Nothing
lookupWith p ((x, y):xs) | p x = Just (x, y)
                         | otherwise = lookupWith p xs
