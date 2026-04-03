module MyParsers.AttoparsecEli where

import MyTypes.MyMaybe
import Data.Attoparsec.Text
import Data.Char (isAsciiUpper, isAsciiLower, toUpper, toLower)


charAAttoparsec :: Parser Char
charAAttoparsec = char 'A'