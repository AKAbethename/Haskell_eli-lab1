module Lib 
    where

import Data.Char (isSpace)
import qualified Data.Text as T
{-
- Создайте и настройте проект

- Реализацию функций поместить в Lib: 
    Напишите функцию сложения по модулю addMod :: Int -> Int -> Int -> Int, 
                которая принимает три целых числа: два слагаемых и модуль, и возвращает сумму двух чисел по модулю. 
                Используя QuickCheck, проверьте следующие свойства: 

    1. Сложение по модулю: (addMod x y m) modm == (x + y)mod m.
    2. Нейтральный элемент: addMod x 0 m == x mod m.
    3. Коммутативность: addMod x y m == addMod y x m.

    Реализуйте функцию reverseWords :: String -> String, которая переворачивает порядок слов в строке. 
                        Используя QuickCheck, проверьте следующие свойства:
    1. Переворачивание пустой строки дает пустую строку.
    2. Переворачивание строки с одним словом дает ту же строку.
    3. Переворачивание строки с несколькими словами меняет порядок слов.
    4. Двойное применение функции возвращает исходную строку.

- Настроить тесты свойств в Spec таким образом, чтобы исходные случайные данные были содержательными
-}


addMod :: Int -> Int -> Int -> Int
addMod x y m = (x + y) `mod` m

reverseWords :: String -> String
reverseWords " " = " "
reverseWords src = myJoin $ reverse $ getWords $ src


getWords :: String -> [String]  -- разбивает строку на список слов 
getWords str = getWords' [] [] str
        where 
            getWords' acc [] [] = reverse acc  -- конец
            getWords' acc curWord [] = reverse $ (reverse $ curWord) : acc
--            getWords' acc curWord " " = reverse $ ((reverse $ " " ++ curWord) : acc)
            getWords' acc curWord " " = reverse $ "" : (reverse curWord) : acc
            getWords' acc curWord (l:ls) = if l == ' ' then getWords' ((reverse $ curWord) : acc) [] ls else getWords' acc (l:curWord) ls
            

myJoin :: [String] -> String  -- склейка слов в одну строку
myJoin [] = ""
myJoin (word : words) = word ++ (if length words == 0 then myJoin words else " " ++ myJoin words)


trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace


trim2 :: T.Text -> T.Text
trim2 = T.strip


myJoin2 :: [T.Text] -> T.Text
myJoin2 [] = T.empty
myJoin2 [w] = w
myJoin2 (w : ws) = w <> T.singleton ' ' <> myJoin2 ws


getWords2 :: T.Text -> [T.Text]
getWords2 = T.words

reverseWords2 :: T.Text -> T.Text
reverseWords2 src = myJoin2 . reverse . getWords2 $ src
--  | T.all (== ' ') src = src   -- строка из одних пробелов
--  | otherwise = myJoin . reverse . T.words $ src




{-
QuickCheck не всегда генерирует полезные данные для проверки функций
    нужно использовать конкретные типы для для содержательного тестирования

propLook1 k v m = lookup k ((k,v) : m) === Just v

quickCheck (verbose propLook1)
    тестирует не то, что нужно. просто проверяет ()

напрямую запишем типы:

propLook2 k v m = lookup k ((k,v) : m) === Just v
    where types = (k :: Int, v :: Int) -- значение не используется, просто маркерует типы

quickCheck (verbose propLook2)
-}

