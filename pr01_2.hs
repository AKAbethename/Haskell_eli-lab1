-------------------------------------
-- Практические задание 1. Часть 2 --
-------------------------------------

module Pr01_2 where

{-

Напишите реализацию функций:
-- myZipSave - попарное объединение двух списков в список пар и сохранение хвоста более длинного списка 
-- myUnzipSave - разделение списка пар на пару списков с восстановлением более длинного списка если исходные списки были разного размера
-- myReverse - разворот списка с использованием сверток
-- myFoldl1 - левая свертка для не пустых списков (без инициирующего значения)
-- myFoldr1 - правая свертка для не пустых списков (без инициирующего значения)
-- myTakeWhile - реализовать с использованием сверток
-- mySpan - реализовать с использованием сверток
-- myMaybe - обработка возможно отсутствующего значения или возвращение значение по умолчанию (maybe)
-- myMap - реализуйте функцию map с использованием типа MyList из материалов лекции
-- myUnFoldr - развертка (операция обратная к свертке)

-- Расширьте типы для выпекания тортов из материалов лекции:
    -- Добавить возможность испечь не менее трех типов тортов
    -- Контроль числа и объема используемых ингредиентов
    -- Обработку недостатка или отсутствия ингредиентов

-}



myProduct :: (b -> a -> b) -> b -> [a] -> b

myProduct f y (x:xs) = myProduct f (y `f` x) (xs)
myProduct f y [] = y


halve :: Integer -> Integer
halve a = a `div` 2 

printDouble :: Integer -> String
printDouble x = show (x * 2)



myZipSave :: [a] -> [a] -> ([(a, a)], [a])
myZipSave (x : xs) (y : ys) = localfunc (x : xs) (y : ys) ans

                where localfunc (x : xs) (y : ys) ans = localfunc xs ys ((x, y) : ans) 
                      localfunc [] (y : ys) ans = (reverse ans, (y:ys))
                      localfunc (x : xs) [] ans = (reverse ans, (x:xs))
                      localfunc [] [] ans = (reverse ans, [])
                      
                      ans = []

myZipSave [] (y : ys) = ([], y : ys)
myZipSave (x : xs) [] = ([], x : xs)
myZipSave [] [] = ([], [])

                    


myUnzipSave :: ([(a, a)], [a]) -> ([a], [a])



myUnzipSave ((z : zs), (r : rs)) = localfunc2 ((z : zs), (r : rs)) ans 
                where localfunc2 ((z : zs), (r : rs)) ans = localfunc2(zs, (r : rs)) (((fst z) : (fst ans)), ((snd z) : (snd ans)))

                      localfunc2 ([], (r : rs)) ans = localfunc2 ([], []) (fst ans, (reverse (r : rs)) ++ snd ans)

                      localfunc2 ([], []) ans = (reverse (fst ans), reverse (snd ans))

                      localfunc2 ((z : zs), []) ans = localfunc2(zs, []) (((fst z) : (fst ans)), ((snd z) : (snd ans)))

                      ans = ([], [])

myUnzipSave ([], (r : rs)) = ([], r : rs)
--myUnzipSave (z : zs, []) = myUnzipSave (zs, [])

myUnzipSave (z : zs, []) = locf (z : zs, []) ans
                where locf (z : zs, []) ans = locf (zs, []) ((fst z : fst ans), (snd z : snd ans))
                      locf ([], []) ans = (reverse (fst ans), reverse (snd ans)) 
                      ans = ([], []) 
myUnzipSave ([], []) = ([], []) 


--myReverse :: ((b -> a -> b) -> b -> a -> b)

--myReverse 


double :: [Int] -> [Int]

--double list = map (\x -> x * 2) list

double = map (\x -> x * 2)


