import Control.Applicative ((<$>), (<*>), (*>))
import Text.ParserCombinators.Parsec

quotedStringParser :: GenParser Char st String
quotedStringParser = quotedString '\'' <|> quotedString '"'

quotedString :: Char -> GenParser Char st String
quotedString x = do
  char x
  inner <- concat <$> (many $ escapedOrStringChars x)
  char x
  return $ x : inner ++ [x]

escapedOrStringChars :: Char -> GenParser Char st String
escapedOrStringChars sep = do
  try escapedChar <|> (many1 $ noneOf ['\\', sep])

escapedChar :: GenParser Char st String
escapedChar = do
  leader <- char '\\'
  ch <- anyChar
  return [leader, ch]

main = getContents >>= \d -> case parse quotedStringParser "<stdin>" d of
  Left err -> print err >> error "Failed."
  Right s -> putStrLn s
