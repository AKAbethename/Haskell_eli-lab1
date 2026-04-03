module MyParsers.ParsecEli where

import MyTypes.MyMaybe

import Text.Parsec
import Text.Parsec.String (Parser)  -- Parser для String
import Data.Char (isLower, isUpper, digitToInt, isDigit)
import Control.Applicative (Alternative(..), optional, ZipList(..))

charA :: Parser Char
charA = char 'A'


mySatisfy :: (Char -> Bool) -> Parser Char
mySatisfy p = satisfy p

myChar :: Char -> Parser Char
myChar c = char c


myLower :: Parser Char
myLower = lower  


myDigit :: Parser Int
myDigit = digitToInt <$> digit


runParser :: Parser a -> String -> MyMaybe (String, a)
runParser p input = 
    case parse p "" input of
        Left _ -> MyNothing
        Right result -> MyJust (input, result)