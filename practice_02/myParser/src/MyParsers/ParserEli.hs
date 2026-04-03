{-# LANGUAGE InstanceSigs #-}

module MyParsers.ParserEli where

import MyTypes.MyMaybe

import Data.Char (isLower, isUpper, digitToInt, isDigit)
import Control.Applicative (Alternative(..), optional, ZipList(..))

newtype Parser tok a = Parser {runParser :: [tok] -> MyMaybe ([tok], a) }

charA :: Parser Char Char 
charA = Parser f where
--  f :: [Char] -> Maybe ([Char], Char)
    f (x:xs) | x == 'A' = MyJust (xs, 'A')
    f _                = MyNothing



satisfy :: (Char -> Bool) -> Parser Char Char
satisfy p = Parser f where
--  f :: [Char] -> Maybe ([Char], Char)
    f (x:xs) | p x = MyJust (xs, x)
    f _                = MyNothing


char :: Char -> Parser Char Char 
char c = Parser f where
--    f :: [Char] -> Maybe ([Char], Char)
    f (x:xs) | x == c = MyJust (xs, c)
    f _                = MyNothing


lower :: Parser Char Char
lower = Parser f where
    f (x:xs) | isLower x = MyJust (xs, 'A')
    f _                = MyNothing



instance Functor (Parser tok) where
    fmap :: (a -> b) -> Parser tok a -> Parser tok b
    fmap g (Parser u) = Parser f where
        -- f :: [tok] -> Maybe ([tok], b)
        f xs = case u xs of
            MyNothing -> MyNothing
            MyJust (toks', x) -> MyJust (toks', g x)

digit :: Parser Char Int
digit = digitToInt <$> satisfy isDigit


instance Applicative (Parser tok) where
    pure :: a -> Parser tok a
    pure x = Parser $ \xs -> MyJust (xs, x)


    (<*>) :: Parser tok (a -> b) -> Parser tok a -> Parser tok b
    Parser u <*> Parser v = Parser f where
        f xs = case u xs of
            MyNothing -> MyNothing
            MyJust (tok, x) -> case v tok of
                MyNothing -> MyNothing
                MyJust (tok', x') -> MyJust (tok', x x')


multiplication :: Parser Char Int
multiplication = (*) <$> digit <* char '*' <*> digit


instance Alternative (Parser tok) where
    empty :: Parser tok a
    empty = Parser $ \_ -> MyNothing

    (<|>) :: Parser tok a -> Parser tok a -> Parser tok a
    Parser u <|> Parser v = Parser f where
        f xs = case u xs of 
            MyNothing -> v xs
            x -> x


lowers :: Parser Char String
lowers = (:) <$> lower <*> lowers <|> pure ""


digits :: Parser Char Int
digits = fmap (foldl (\x y -> x * 10 + y) 0) $ some digit

finalMult :: Parser Char Int
finalMult = (*) <$> digits <* char '*' <*> digits

finalPlus :: Parser Char Int
finalPlus = (+) <$> digits <* char '+' <*> digits


plusOrMult :: Parser Char Int
plusOrMult = finalMult <|> finalPlus


{-
runParser plusOrMult "12*345dsf"
runParser plusOrMult "12+345dsf"
-}