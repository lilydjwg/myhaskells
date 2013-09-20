import Prelude hiding (lex)
import System.IO (hPutStrLn, stderr)
import Language.Python.Common.SrcLocation (span_column)
import Language.Python.Common.Token (Token(IndentToken), token_span)
import Language.Python.Version3 (lex)

-- This is several (~5x) faster than the following:
--   python -c 'import sys, token, tokenize; print(min(len(x.string) for x in tokenize.tokenize(sys.stdin.buffer.readline) if x.type == token.INDENT))'
-- But it can't handle multibyte characters (https://github.com/bjpop/language-python/issues/7)

minimumOr5 [] = 5
minimumOr5 x = minimum x

minIndent = flip (-) 1 . minimumOr5 . map (span_column . token_span) . filter isIndentToken
  where isIndentToken (IndentToken _) = True
        isIndentToken _ = False

main = getContents >>= either showErr (print . minIndent) . (flip lex "<stdin>")
  where showErr = hPutStrLn stderr . show
