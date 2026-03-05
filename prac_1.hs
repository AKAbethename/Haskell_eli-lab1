-------------------------------------
-- Практические задание 1. Часть 1 --
-------------------------------------

module Pr01_1 where

{-

Напишите реализацию функций myFST, mySND, myTHRD для кортежа (a,b,c)  DONE

Напишите реализацию стандартных функции для работы со списками:
-- myHead - определение (через сопоставление с образцом) функции отделения головы списка  DONE
-- myTail - функция отделения хвоста списка   DONE
-- myTake - взять первые n элементов списка   DONE
-- myDrop - отбросить первые n элементов списка  DONE
-- myProduct - перемножить все элементы списка  DONE
-- myZip - попарное объединение двух списков в список пар, длина итогового списка по длине самого короткого из входных списков  DONE
-- myZip3 объединение трех списков в список троек   DONE
-- myUnzip - разделение списка пар на пару списков  DONE

Напишите реализацию стандартных функции высшего порядка для работы со списками:
-- myFilter - применение предиката к каждому элементу списка (две реализации: с использованием охранных выражений и if-then-else)  DONE
-- myMap - применение функции одного аргумента к каждому элементу списка    DONE
-- myZipWith - применение функции двух аргументов к двум спискам    DONE
-- myZipWith3 - применение функции трех аргументов к трем спискам   DONE
-- myAll - проверяет удовлетворяют ли все элементы списка предикату   DONE
-- myAny - проверяет удовлетворяют ли хотя бы один элемент списка предикату   DONE
-- myComposition - композиция двух функций (.)   DONE

-}

myFST :: (a, b, c) -> a
myFST (x, y, z) = x


mySND :: (a, b, c) -> b
mySND (x, y, z) = y


myTHRD :: (a, b, c) -> c
myTHRD (x, y, z) = z


myHead :: [a] -> a
myHead (x:_) = x
myHead [] = error "Пустой список!"


myTail :: [a] -> [a]
myTail (x:liste) = liste
myTail [] = error "Пустой список!"

{-
myTake :: [a] -> Int -> [a] -> [a]   -- копить будем
myTake stroka 0 ans = reverse ans
myTake (x : tail_eli) n ans = myTake tail_eli (n-1) (x : ans)
myTake [] n ans = if n > 0
                then error "Es inch a?"
                else []
-}

myTake :: Int -> [a] -> [a]
myTake 0 (x:xs) = []
myTake n [] = []
myTake n (x:xs) = x : (myTake (n-1) xs)

factorial :: Int -> Int
factorial 0 = 1  
factorial n = n * factorial (n-1)  -- общий случай перехватывает всё



{-
myDrop :: [a] -> Int -> [a]
myDrop stroka 0 = stroka 
myDrop (x : tail_eli) n = myDrop (tail_eli) (n-1)
myDrop [] n = if n > 0
                    then error "Хотим выбросить больше элементов, чем имеем!"
                    else []
-}

myDrop :: Int -> [a] -> [a]
myDrop 0 list = list
myDrop n (x:xs) = myDrop (n-1) (xs)


myProduct :: [Int] -> Int
myProduct (x : other) = x * myProduct (other)
myProduct [] = 1

{-
myZip :: [a] -> [a] -> [(a, a)] -> [(a, a)]  -- Будем копить
myZip (x : otherx) (y : othery) ans = myZip (otherx) (othery) ((x, y) : ans)
myZip [] (y : othery) ans = reverse ans
myZip (x : otherx) [] ans = reverse ans
myZip [] [] ans = reverse ans
-}


myZip :: [a] -> [b] -> [(a, b)]
myZip [] [] = []
myZip xs [] = []
myZip [] ys = []
myZip (x:xs) (y:ys) = (x, y) : (myZip xs ys)


{-

myZip3 :: [a] -> [a] -> [a] -> [(a, a, a)] -> [(a, a, a)]
myZip3 (x : otherx) (y : othery) (z : otherz) ans = myZip3 (otherx) (othery) (otherz) ((x, y, z) : ans)

myZip3 [] (y : othery) (z : otherz) ans = reverse ans
myZip3 (x : otherx) [] (z : otherz) ans = reverse ans
myZip3 (x : otherx) (y : othery) [] ans = reverse ans

myZip3 [] [] (z : otherz) ans = reverse ans
myZip3 (x : otherx) [] [] ans = reverse ans
myZip3 [] (y : othery) [] ans = reverse ans


myZip3 [] [] [] ans = reverse ans

-}

myZip3 :: [a] -> [b] -> [c] -> [(a, b, c)]

myZip3 [] [] [] = []
myZip3 list1 [] [] = []
myZip3 [] list2 [] = []
myZip3 [] [] list3 = []
myZip3 list1 list2 [] = []
myZip3 list1 [] list3 = []
myZip3 [] list2 list3 = []
myZip3 (x:xs) (y:ys) (z:zs) = (x, y, z) : (myZip3 (xs) (ys) (zs))



{-
myUnzip :: [a] -> ([a], [a]) -> ([a], [a])
myUnzip (x : y : other) ans = if length other > 1 
                                then myUnzip (other) (x : fst ans, y : snd ans)
                                else if length other == 1 
                                    then (reverse (myHead other : x : fst ans), reverse (y : snd ans))
                                    else (reverse (x : fst ans), reverse (y : snd ans))


-}


myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip (x:xs) = ((fst x) : (fst (myUnzip xs)), ((snd x) : (snd (myUnzip xs))))



{-  Задание 2 -}

myPred :: Int -> Bool   -- дает остаток 2 при делении на 3
myPred x = if (x == 1)
                then False
                else if (x == 2)
                    then True
                    else if (x == 0)
                        then False
                        else myPred (x - 3)


{-

myFilter :: (Int -> Bool) -> [Int] -> [Int]
myFilter myPred (x : xs) = if (myPred x)
                            then x : myFilter myPred (xs)
                            else myFilter myPred (xs)
myFilter myPred [] = []

-}


{-
myFilter :: (Int -> Bool) -> [Int] -> [Int]
myFilter myPred (x : xs) | (myPred x)  =  x : myFilter myPred (xs)
                         | (myPred x == False)  =  myFilter myPred (xs)
myFilter myPred [] = []
-}



myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p (x:xs) = if (p x) then x : (myFilter p xs) else (myFilter p xs)
myFilter p [] = []


myFilter2 :: (a -> Bool) -> [a] -> [a]
myFilter2 p (x:xs)  | p x = x : (myFilter p xs)
                    | otherwise = myFilter p xs
 

anyFunc :: Int -> Int
anyFunc x = x * 3

{-
myMap :: (Int -> Int) -> [Int] -> [Int]
myMap anyFunc (x : xs) = (anyFunc x) : (myMap anyFunc xs)
myMap anyFunc [] = []
-}

myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = (f x) : (myMap f xs)
myMap f [] = []

-- myZipWith - применение функции двух аргументов к двум спискам

anyFunc2 :: Int -> Int -> Int
anyFunc2 x y = x * y

{-

myZipWith :: (Int -> Int -> Int) -> [Int] -> [Int] -> [Int]
myZipWith anyFunc2 (x : xs) (y : ys) = (anyFunc2 x y) : (myZipWith anyFunc2 (xs) (ys))
myZipWith anyFunc2 [] (y : ys) = (anyFunc2 1 y) : (myZipWith anyFunc2 [] (ys))
myZipWith anyFunc2 (x : xs) [] = (anyFunc2 x 1) : (myZipWith anyFunc2 (xs) [])
myZipWith anyFunc2 [] [] = []
-}

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

myZipWith f (x:xs) (y:ys) = (f x y) : (myZipWith f (xs) (ys))
myZipWith f [] (y:ys) = []
myZipWith f (x:xs) [] = []
myZipWith f [] [] = []


-- myZipWith3 - применение функции трех аргументов к трем спискам


anyFunc3 :: Int -> Int -> Int -> Int
anyFunc3 x y z = x + y + z


{-

myZipWith3 :: (Int -> Int -> Int -> Int) -> [Int] -> [Int] -> [Int] -> [Int]
myZipWith3 anyFunc3 (x : xs) (y : ys) (z : zs) = (anyFunc3 x y z) : (myZipWith3 anyFunc3 (xs) (ys) (zs))

myZipWith3 anyFunc3 [] (y : ys) (z : zs) = (anyFunc3 0 y z) : (myZipWith3 anyFunc3 [] (ys) (zs))
myZipWith3 anyFunc3 (x : xs) [] (z : zs) = (anyFunc3 x 0 z) : (myZipWith3 anyFunc3 (xs) [] (zs))
myZipWith3 anyFunc3 (x : xs) (y : ys) [] = (anyFunc3 x y 0) : (myZipWith3 anyFunc3 (xs) (ys) [])

myZipWith3 anyFunc3 [] [] (z : zs) = (anyFunc3 0 0 z) : (myZipWith3 anyFunc3 [] [] (zs))
myZipWith3 anyFunc3 [] (y : ys) [] = (anyFunc3 0 y 0) : (myZipWith3 anyFunc3 [] (ys) [])
myZipWith3 anyFunc3 (x : xs) [] [] = (anyFunc3 x 0 0) : (myZipWith3 anyFunc3 (xs) [] [])

myZipWith3 anyFunc3 [] [] [] = []

-}

myZipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]

myZipWith3 f (x:xs) (y:ys) (z:zs) = (f x y z) : (myZipWith3 f xs ys zs)
myZipWith3 f [] (y:ys) (z:zs) = []
myZipWith3 f (x:xs) [] (z:zs) = []
myZipWith3 f (x:xs) (y:ys) [] = []
myZipWith3 f [] [] (z:zs) = []
myZipWith3 f (x:xs) [] [] = []
myZipWith3 f [] (y:ys) [] = []
myZipWith3 f [] [] [] = []


{-
myAll :: (Int -> Bool) -> [Int] -> Bool
myAll myPred (x : xs) = if (myPred x == False) then False else myAll myPred (xs)
myAll myPred [] = True 
-}

myAll :: (a -> Bool) -> [a] -> Bool
myAll p (x:xs) = (p x) && (myAll p xs)
myAll p [] = True


{-
myAny :: (Int -> Bool) -> [Int] -> Bool
myAny myPred (x : xs) = if(myPred x == True) then True else myAny myPred (xs)
myAny myPred [] = False
-}


myAny :: (a -> Bool) -> [a] -> Bool
myAny p (x:xs) = (p x) || (myAny p xs)
myAny p [] = False

{-
myComposition :: (b -> c) -> (a -> b) -> a -> c
myComposition = \f g x -> f (g x)

f :: Int -> Int
f x = x * 2

g :: Int -> Int
g x = x + 1

-}




myComposition :: (b -> c) -> (a -> b) -> a -> c
myComposition f g = (\x -> f (g x))






