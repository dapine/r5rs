module Parser
    ( symbol
    , spaces
    , parseString
    , parseAtom
    , parseNumber
    , parseHex
    , parseOct
    , parseExpr
    , parseList
    , parseDottedList
    , parseQuoted
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool deriving (Eq, Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseString' :: Parser LispVal
parseString' = do
    char '"'
    x <- many (alphaNum <|> symbol <|> char '\n' <|> char '\r' <|> char '\t' <|> char '\\')
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
               "#t" -> Bool True
               "#f" -> Bool False
               _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseNumber' :: Parser LispVal
parseNumber' = do
    x <- many1 digit
    return $ Number $ read x

parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= \x -> return $ Number $ read x

parseHex :: Parser LispVal
parseHex = do
    char '#'
    char 'x'
    x <- many1 hexDigit
    return $ Number $ fst . head . readHex $ x

parseOct :: Parser LispVal
parseOct = do
    char '#'
    char 'o'
    x <- many1 octDigit
    return $ Number $ fst . head . readOct $ x

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]
