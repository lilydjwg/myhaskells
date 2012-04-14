filterIO :: (String -> String) -> IO ()
filterIO f = interact $ unlines . map f . lines

expand :: String -> String
expand s = let (word:times:[]) = words s
           in unwords $ replicate (read times) word

main = filterIO expand
