module Lib
    ( doubleAll
    , filterEvens
    ) where

-- Экспортируемые функции
doubleAll :: Num a => [a] -> [a]
doubleAll = map (*2)

filterEvens :: Integral a => [a] -> [a]
filterEvens = filter even

-- Внутренняя функция (не экспортируется)
internalFunc :: Int -> Int
internalFunc x = x * x + 1