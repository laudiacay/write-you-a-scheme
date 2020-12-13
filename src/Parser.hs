module Parser (
  readExpr
              )
  where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric (readOct, readHex, readFloat)
import Data.Char (digitToInt)



data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Float Float
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

{-
parseNumber' :: Parser LispVal
parseNumber' = do
  digits <- many1 digit
  return ((Number . read) digits)

parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= (return . Number . read)
-}

{-Characters are objects that represent printed characters such as letters and digits. Characters are written using the notation #\<character> or #\<character name>. For example:

#\a	; lower case letter
#\A	; upper case letter
#\(	; left parenthesis
#\	; the space character
#\space	; the preferred way to write a space
#\newline	; the newline character
Case is significant in #\<character>, but not in #\<character name>. If <character> in #\<character> is alphabetic, then the character following <character> must be a delimiter character such as a space or parenthesis. This rule resolves the ambiguous case where, for example, the sequence of characters ``#\space'' could be taken to be either a representation of the space character or a representation of the character ``#\s'' followed by a representation of the symbol ``pace.''

Characters written in the #\ notation are self-evaluating. That is, they do not have to be quoted in programs. Some of the procedures that operate on characters ignore the difference between upper case and lower case. The procedures that ignore case have ``-ci'' (for ``case insensitive'') embedded in their names.
-}
parseChar :: Parser LispVal
parseChar = do
  char '#'
  char '\\'
  rest <- (string "space") <|> (string "newline") <|> (anyChar >>= (\x -> return  [x]))
  return (Character $ case rest of
    "space" -> ' '
    "newline" -> '\n'
    c -> c !! 0)

parseFloat :: Parser LispVal
parseFloat = do
  whole_num <- many1 digit
  char '.'
  fractional_bit <- many1 digit
  return . Float . fst $ (readFloat (whole_num ++ "." ++ fractional_bit) !! 0 )

parseExpr :: Parser LispVal
parseExpr = try parseAtom
  <|> try parseChar
  <|> try parseString
  <|> try parseFloat
  <|> try parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err ++ ", input: " ++ input
    Right val -> "Found value: " ++ show val
