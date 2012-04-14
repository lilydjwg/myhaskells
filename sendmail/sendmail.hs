import System.IO
import System.Directory (getHomeDirectory)
import System.FilePath (joinPath, FilePath)
import System.Posix.User (getLoginName)
import System.Time (getClockTime)

-- FIXME Important Notice: This might not be able to handle multibytes

mailbox :: IO FilePath
mailbox = do
  home <- getHomeDirectory
  login <- getLoginName
  let inbox = case login of
		   "lilydjwg" -> ".Mail/inbox"
		   otherwise -> "cron.letter"
  return $ joinPath [home, inbox]

processMessage :: String -> IO ()
processMessage mailbox = withFile mailbox AppendMode (\fd -> do
  now <- getClockTime
  hPutStrLn fd $ "From cron@lilyforest " ++ show now
  hPutStrLn fd "Return-Path: <cron@lilyforest>"
  hPutStrLn fd "Delivered-To: localhost"
  hPutStrLn fd $ "Date: " ++ show now
  getContents >>= hPutStr fd
  )

main = processMessage =<< mailbox
