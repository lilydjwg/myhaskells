module Text.Lrc (
  parseLrc,
  addTime,
  lrcAddOffset,
  Lrc(..),
  LrcLine(..),
) where

import Data.Char (isDigit)
import Data.Functor ((<$>))
import Data.List (sort)
import Data.Maybe (isJust, fromJust)
import Text.ParserCombinators.Parsec

import Text.String (parseInt)

data Lrc = Lrc {
  title :: Maybe String,
  artist :: Maybe String,
  album :: Maybe String,
  by :: Maybe String,
  metadata :: [(String, String)],
  lyrics :: [LrcLine]
}

data LrcLine = LrcLine {
  time :: Int,
  line :: String
} deriving (Eq, Show, Ord)

parseLrc = parse lrcParser

lrcParser :: GenParser Char st Lrc
lrcParser = do
  metadata <- many $ try lrcMeta
  ly <- concat <$> many lrcLine
  return Lrc {
    title = lookup "ti" metadata,
    artist = lookup "ar" metadata,
    album = lookup "al" metadata,
    by = lookup "by" metadata,
    metadata = metadata,
    lyrics = sort ly
  }

lrcMeta :: GenParser Char st (String, String)
lrcMeta = do
  char '['
  key <- many $ noneOf ":"
  char ':'
  val <- many $ noneOf "]"
  char ']'
  eol
  return (key, val)

lrcLine :: GenParser Char st [LrcLine]
lrcLine = do
  times <- many1 lrcTime
  line <- many $ noneOf "\r\n"
  optional eol
  return $ map (\t -> LrcLine {
    time = t,
    line = line
  }) times

lrcTime :: GenParser Char st Int
lrcTime = do
  char '['
  minutes <- readInt
  char ':'
  second <- readInt
  centisec <- option 0 $ char '.' >> readInt
  char ']'
  return $ 60 * 100 * minutes + 100 * second + centisec
  where readInt = read <$> many digit

eol :: GenParser Char st String
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"

lrcAddOffset :: Lrc -> Lrc
lrcAddOffset l = l { lyrics = ly', metadata = meta' }
  where ly = lyrics l
        meta = metadata l
        offset = lookup "offset" meta >>= parseInt
        ly' = case offset of
                   Just t -> addTime (fromInteger t `div` 10) ly
                   otherwise -> ly
        meta' = filter notOffset meta
        notOffset = (/= "offset") . fst

addTime :: Int -> [LrcLine] -> [LrcLine]
addTime t = map $ \l -> l { time = (t + time l) }

main = getContents >>= \lrcfile -> case parse lrcParser "<stdin>" lrcfile of
  Left err -> print err >> error "Failed."
  Right lrc -> mapM_ print $ lyrics $ lrcAddOffset lrc
