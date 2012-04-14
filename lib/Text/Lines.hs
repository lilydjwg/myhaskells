module Text.Lines (
  getIndent,
) where

getIndent :: [String] -> Int
getIndent = minimum . (map (length . (takeWhile (==' '))))
