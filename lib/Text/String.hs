module Text.String (
  dropPrefix,
  isSpace,
  stripl,
  parseInt,
  tr,
  trChar,
  trSingle,
  trCharSingle,
) where

import Data.List (elemIndex, isPrefixOf)

isSpace :: Char -> Bool
isSpace = (`elem` " \t\n\r\v")

stripl :: String -> String
stripl = dropWhile isSpace

parseInt :: String -> Maybe Integer
parseInt s = case reads s of
  [(int, "")] -> Just int
  otherwise   -> Nothing

tr :: [Char] -> [Char] -> String -> String
tr from to = map (trSingle from to)

trSingle :: [Char] -> [Char] -> Char -> Char
trSingle from to ch = case i of
                         Just i -> to !! i
                         _      -> ch
                         where i = elemIndex ch from

trChar :: Char -> Char -> String -> String
trChar from to = map (trCharSingle from to)

trCharSingle :: Char -> Char -> Char -> Char
trCharSingle from to ch = if from == ch then to else ch

dropPrefix :: String -> String -> Either String String
dropPrefix p s = if p `isPrefixOf` s
                    then Right $ drop (length p) s
                    else Left s
