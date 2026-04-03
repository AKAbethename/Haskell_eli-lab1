module MyParsers.ParsecEli where

import MyTypes.MyMaybe

import Text.Parsec
import Text.Parsec.String (Parser)  -- Parser для String

charA :: Parser Char
charA = char 'A'


mySatisfy :: (Char -> Bool) -> Parser Char
mySatisfy p = satisfy p