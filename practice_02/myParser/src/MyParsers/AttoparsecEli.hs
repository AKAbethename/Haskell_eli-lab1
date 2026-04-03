module MyParsers.AttoparsecEli where

import MyTypes.MyMaybe
import Data.Attoparsec.Text
import Data.Char (isLower, isUpper, digitToInt, isDigit, isAsciiLower )
import Control.Applicative (Alternative(..))  -- для <|>
import Data.Text as T


charAAttoparsec :: Parser Char
charAAttoparsec = char 'A'




satisfyAttoparsec :: (Char -> Bool) -> Parser Char
satisfyAttoparsec p = satisfy p

charAttoparsec :: Char -> Parser Char
charAttoparsec c = char c


lowerAttoparsec :: Parser Char
lowerAttoparsec = satisfy isAsciiLower  


digitAttoparsec :: Parser Int
digitAttoparsec = digitToInt <$> digit

digitsAttoparsec :: Parser Int
digitsAttoparsec = read <$> many1 digit



finalMultAttoparsec :: Parser Int
finalMultAttoparsec = (*) <$> digitsAttoparsec <* char '*' <*> digitsAttoparsec


finalPlusAttoparsec :: Parser Int
finalPlusAttoparsec = (+) <$> digitsAttoparsec <* char '+' <*> digitsAttoparsec


plusOrMultAttoparsec :: Parser Int  
plusOrMultAttoparsec = finalMultAttoparsec <|> finalPlusAttoparsec

runParser :: Parser a -> String -> MyMaybe (String, a)
runParser p input = 
     case parseOnly p (T.pack input) of
        Left _ -> MyNothing
        Right result -> MyJust ("", result)
