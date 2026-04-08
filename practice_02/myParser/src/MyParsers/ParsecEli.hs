module MyParsers.ParsecEli where

import MyTypes.MyMaybe

import Text.Parsec
import Text.Parsec.String (Parser) 
import Data.Char (isLower, isUpper, digitToInt, isDigit)
-- import Control.Applicative (Alternative(..), optional, ZipList(..))
import qualified Control.Applicative as App

import Text.Parsec (try)

charAParsec :: Parser Char
charAParsec = char 'A'


satisfyParsec :: (Char -> Bool) -> Parser Char
satisfyParsec p = satisfy p



charParsec :: Char -> Parser Char
charParsec c = char c

-- char c              = satisfy (==c)  <?> show [c]


lowerParsec :: Parser Char
lowerParsec = lower  

-- lower               = satisfy isLower


digitParsec :: Parser Int
digitParsec = digitToInt <$> digit

digitsParsec :: Parser Int
digitsParsec = read <$> many1 digit


finalMultParsec :: Parser Int
finalMultParsec = (*) <$> digitsParsec <* char '*' <*> digitsParsec


finalPlusParsec :: Parser Int
finalPlusParsec = (+) <$> digitsParsec <* char '+' <*> digitsParsec



plusOrMultParsec :: Parser Int 
--plusOrMultParsec = finalMultParsec <|> finalPlusParsec
plusOrMultParsec = try finalMultParsec <|> finalPlusParsec


runParser :: Parser a -> String -> MyMaybe (String, a)
runParser p input = 
    case parse p "" input of
        Left _ -> MyNothing
        Right result -> MyJust ("", result)


