module Main where

import Parser
import Eval
import Text.ParserCombinators.Parsec (parse)
import System.Environment (getArgs)

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> String $ "No match: " ++ show err
                   Right val -> val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
