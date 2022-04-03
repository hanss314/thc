module Lexer where

import Parser
import Control.Applicative


data LexerToken = T_LPAREN
                | T_RPAREN
                | T_LBRACE
                | T_RBRACE
                | T_LBRACKET
                | T_RBRACKET
                | T_EQUALS
                | T_WHITESPACE Int
                | T_NEWLINE
                | T_SEMICOLON
                | T_COMMA
                | T_PN
                | T_COLON
                | T_BSLASH

                | T_DATA
                | T_NEWTYPE
                | T_INSTANCE
                | T_CLASS
                | T_WHERE
                | T_DERIVING
                | T_IMPORT
                | T_MODULE
                
                | T_String String
                | T_Char String
                | T_IDLower String
                | T_IDUpper String
                | T_IDSymbol String
                | T_IDForceInfix String
                | T_Int Integer
                deriving (Eq, Show)



asciiSymbol :: Parser Char Char
asciiSymbol = satisfyingOne (`elem` "!#$%&*+./<=>?@\\^|-~")

asciiUpper = satisfyingOne (`elem` ['A'..'Z'])
asciiLower = satisfyingOne (`elem` '_':['a'..'z'])
asciiNum = satisfyingOne (`elem` ['0'..'9'])
asciiNonZero = satisfyingOne (`elem` ['1'..'9'])

alphNum :: Parser Char Char
alphNum = asciiUpper <|> asciiLower <|> asciiNum

stringRegex :: Parser Char String
stringRegex = supQuote *> (fmap concat $ many (nonBS <|> escape)) <* supQuote  where
    nonBS = fmap pure (satisfyingOne (\c -> c /= '\\' && c /= '"'))
    escape = liftA2 (\a b -> [a,b]) (matchOne '\\') getOne
    supQuote = suppress $ matchOne '"'

charRegex :: Parser Char String
charRegex = supQuote *> (nonBS <|> escape) <* supQuote  where
    nonBS = fmap pure (satisfyingOne (\c -> c /= '\\' && c /= '\''))
    escape = liftA2 (\a b -> [a,b]) (matchOne '\\') getOne
    supQuote = suppress $ matchOne '\''

idLowerRegex = liftA2 (:) asciiLower $ many alphNum
idUpperRegex = liftA2 (:) asciiUpper $ many alphNum

lexerClasses :: [Parser Char LexerToken]
lexerClasses = [
    matchConst T_LPAREN     "(",
    matchConst T_RPAREN     ")",
    matchConst T_LBRACE     "{",
    matchConst T_RBRACE     "}",
    matchConst T_LBRACKET   "[",
    matchConst T_RBRACKET   "]",
    matchConst T_EQUALS     "=",
    fmap (T_WHITESPACE . length) $ some $ matchOne ' ',
    matchConst T_NEWLINE    "\n",
    matchConst T_SEMICOLON  ";",
    matchConst T_COMMA      ",",
    matchConst T_PN         "::",
    matchConst T_COLON      ":",
    matchConst T_BSLASH     "\\",

    matchConst T_DATA       "data",
    matchConst T_NEWTYPE    "newtype",
    matchConst T_INSTANCE   "instance",
    matchConst T_CLASS      "class",
    matchConst T_WHERE      "where",
    matchConst T_DERIVING   "deriving",
    matchConst T_IMPORT     "import",
    matchConst T_MODULE     "module",
    
    fmap T_String stringRegex,
    fmap T_Char charRegex,
    fmap T_IDLower $ idLowerRegex,
    fmap T_IDUpper $ idUpperRegex,
    fmap T_IDSymbol $ some asciiSymbol,
    fmap T_IDForceInfix ((matchOne '`') *> idLowerRegex <* (matchOne '`')),
    fmap (T_Int . read) $ liftA2 (:) asciiNonZero $ many asciiNum]

cleanWhitespace :: [LexerToken] -> [LexerToken]
cleanWhitespace l = case l of
    ((T_WHITESPACE _):xs) -> cleanWhitespace xs
    (T_NEWLINE:(T_WHITESPACE n):xs) -> T_NEWLINE : (T_WHITESPACE n) : cleanWhitespace xs
    (x:xs) -> x : cleanWhitespace xs
    [] -> []

thcLexer :: Parser Char [LexerToken]
thcLexer = fmap cleanWhitespace $ many $ foldr (<|>) empty lexerClasses
