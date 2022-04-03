import System.IO
import System.Environment

import Data.Maybe

import Parser (runParser)
import Lexer (thcLexer, LexerToken)


printParseResult :: (String, [LexerToken]) -> IO ()
printParseResult (rem, toks) = do
    mapM_ print toks
    putStrLn "Remaining:"
    print rem

main = do
    args <- getArgs
    let fileName = head args
    contents <- readFile fileName

    let parseResult = runParser thcLexer contents
    maybe (print "No Lex") printParseResult parseResult
    
