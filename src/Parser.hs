module Parser
    ( symbol
    , spaces
    , parseString
    , parseCharacter
    , parseAtom
    , parseNumber
    , parseDecimal
    , parseHex
    , parseOct
    , parseFloat
    , parseRational
    , parseExpr
    , parseList
    , parseDottedList
    , parseQuoted
    , LispVal ( Atom, List, DottedList
              , Number, Float, Rational
              , String, Bool, Character
              )
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Rational { nom :: Integer, denom :: Integer }
             | String String
             | Bool Bool
             | Character Char deriving (Eq)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

special :: Parser Char
special = oneOf "()[],.;{}"

lspace :: Parser Char
lspace = string "space" >> return ' '

lnewline :: Parser Char
lnewline = string "newline" >> return '\n'

lchar :: Parser Char
lchar = try lspace <|> try lnewline <|> letter <|> special <|> symbol

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

parseCharacter :: Parser LispVal
parseCharacter = do
    char '#'
    char '\\'
    x <- lchar
    return $ Character x

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
parseNumber = parseDecimal <|> try parseHex <|> try parseOct

parseNumber' :: Parser LispVal
parseNumber' = do
    x <- many1 digit
    return $ Number $ read x

parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= \x -> return $ Number $ read x

parseDecimal :: Parser LispVal
parseDecimal = liftM (Number . read) $ many1 digit

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

parseFloat :: Parser LispVal
parseFloat = do
    first <- many1 digit
    char '.'
    rest <- many1 digit
    return $ Float $ fst . head . readFloat $ first ++ "." ++ rest

parseRational :: Parser LispVal
parseRational = do
    n <- many1 digit
    char '/'
    d <- many1 digit
    return $ Rational (toInteger $ read n) (toInteger $ read d)

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
