module Parser (
  readExpr
              )
  where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric (readOct, readHex)
import Data.Char (digitToInt)



data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
  deriving (Eq, Show)


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

myEscapes :: Parser Char
myEscapes = do
  char '\\'
  char_escaped <- oneOf ['"', 'n', 't', 'r', '\\']
  return $ case char_escaped of
    '\\' -> '\\'
    '"'  -> '"'
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'

-- test escaping quotes with: "\"\\\"\"" hahaha
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (myEscapes <|> noneOf ['\\', '"']) 
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


parseDecimal :: Parser LispVal
parseDecimal = liftM (Number . read) $ many1 digit

readBinRec :: [Int] -> Integer
readBinRec (lsb : rest) = (toInteger lsb) + 2 * readBinRec rest
readBinRec [] = 0

readBinReal :: String -> Integer
readBinReal = readBinRec . reverse . map digitToInt

parseBinary :: Parser LispVal
parseBinary = liftM (Number . readBinReal) $ many1 (oneOf "01")

readOctReal :: String -> Integer
readOctReal s = fst ((readOct s) !! 0)
parseOctal :: Parser LispVal
parseOctal = liftM (Number . readOctReal) $ many1 (oneOf "01234567")

readHexReal :: String -> Integer
readHexReal s = fst ((readHex s) !! 0)
parseHex :: Parser LispVal
parseHex = liftM (Number . readHexReal) $ many1 (oneOf "0123456789ABCDEFabcdef")

parsePrefixedNumber :: Parser LispVal
parsePrefixedNumber = do
  char '#'
  radix <- oneOf ['b', 'o', 'd', 'x']
  case radix of
    'b' -> parseBinary
    'o' -> parseOctal
    'd' -> parseDecimal
    'x' -> parseHex

-- #b (binary), #o (octal), #d (decimal), and #x (hexadecimal)
parseNumber :: Parser LispVal
parseNumber = parsePrefixedNumber <|> parseDecimal

parseNumber' :: Parser LispVal
parseNumber' = do
  digits <- many1 digit
  return ((Number . read) digits)

parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= (return . Number . read)

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err ++ ", input: " ++ input
    Right val -> "Found value: " ++ show val
