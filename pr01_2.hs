-------------------------------------
-- Практические задание 1. Часть 2 --
-------------------------------------

module Pr01_2 where

{-

Напишите реализацию функций:
-- myZipSave - попарное объединение двух списков в список пар и сохранение хвоста более длинного списка 
-- myUnzipSave - разделение списка пар на пару списков с восстановлением более длинного списка если исходные списки были разного размера
-- myReverse - разворот списка с использованием сверток  DONE
-- myFoldl1 - левая свертка для не пустых списков (без инициирующего значения)  DONE
-- myFoldr1 - правая свертка для не пустых списков (без инициирующего значения)  DONE
-- myTakeWhile - реализовать с использованием сверток  DONE
-- mySpan - реализовать с использованием сверток  DONE
-- myMaybe - обработка возможно отсутствующего значения или возвращение значение по умолчанию (maybe)  DONE
-- myMap - реализуйте функцию map с использованием типа MyList из материалов лекции  DONE
-- myUnFoldr - развертка (операция обратная к свертке)  DONE

-- Расширьте типы для выпекания тортов из материалов лекции:
    -- Добавить возможность испечь не менее трех типов тортов
    -- Контроль числа и объема используемых ингредиентов
    -- Обработку недостатка или отсутствия ингредиентов

-}



-- myZipSave - попарное объединение двух списков в список пар и сохранение хвоста более длинного списка 
-- myUnzipSave - разделение списка пар на пару списков с восстановлением более длинного списка если исходные списки были разного размера
-- myReverse - разворот списка с использованием сверток


{-

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

-}

-- myZipSave - попарное объединение двух списков в список пар и сохранение хвоста более длинного списка

helpZipSave :: [a] -> [b] -> ([(a, b)], ([a], [b])) -> ([(a, b)], ([a], [b]))

helpZipSave (x : xs) (y : ys) ans = helpZipSave xs ys ((x, y) : fst ans, snd ans)
helpZipSave [] (y:ys) ans = helpZipSave [] ys (fst ans, (fst (snd ans), y : (snd (snd ans))))
helpZipSave (x:xs) [] ans = helpZipSave xs [] (fst ans, (x : (fst (snd ans)), snd (snd ans)))
helpZipSave [] [] ans = (reverse $ fst ans, (reverse $ fst $ snd ans, reverse $ snd $ snd ans))

{-
myZipSave :: [a] -> [b] -> ([(a, b)], [a])
myZipSave (x:xs) (y:ys) = (fst $ func, if (null $ fst $ snd $ func) then (snd $ snd $ func) else (fst $ snd $ func))

                  where func = helpZipSave (x : xs) (y : ys) ([], ([], []))

-}


--myZipSave :: [a] -> [b] -> ([([a], [b])], [a])

                    


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



-- myReverse - разворот списка с использованием сверток

help_func :: [a] -> a -> [a]
help_func list x = x : list

myReverse :: [a] -> [a]
myReverse xs = foldl help_func [] xs

-- myFoldl1 - левая свертка для не пустых списков (без инициирующего значения)


myFoldl1 :: (a -> a -> a) -> [a] -> Maybe a
myFoldl1 _ [] = Nothing
myFoldl1 f (x:xs) = Just (foldl f x xs)


-- myFoldr1 - правая свертка для не пустых списков (без инициирующего значения)

myFoldr1 :: (a -> a -> a) -> [a] -> Maybe a
myFoldr1 _ [] = Nothing
myFoldr1 f (x:xs) = Just (foldr f x xs)



-- myTakeWhile - реализовать с использованием сверток

--myTakeWhile p = foldr (\x b -> if (p x) then x : b else [] ++ b) []

localTakeWhile :: (a -> Bool) -> [a] -> ([a], [a])
localTakeWhile p = foldl (\b x -> if ((p x) && all p (snd b)) then (((fst b) ++ [x]), snd b) else ((fst b, ((snd b) ++ [x])))) ([], [])

      
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p list = fst (localTakeWhile p list)


-- mySpan - реализовать с использованием сверток

mySpan :: (a -> Bool) -> [a] -> ([a], [a])

mySpan p = foldl (\b x -> if ((p x) && all p (snd b)) then (((fst b) ++ [x], snd b)) else (fst b, ((snd b) ++ [x]))) ([], [])


-- myMaybe - обработка возможно отсутствующего значения или возвращение значение по умолчанию (maybe)
                
myMaybe :: b -> (a -> b) -> Maybe a -> b
--myMaybe def _ Nothing = def
--myMaybe _ f (Just x) = f x

myMaybe def f mx = case mx of 
      Nothing -> def
      Just a -> f a

data MyList a = MyEmpty | MyCons a (MyList a)
--     MyList a — алгебраический тип с конструктором MyEmpty (пустой список) и MyCons (элемент и хвост списка)
mylist = MyCons 3 (MyCons 5 MyEmpty)

mylist2 = MyCons 3 (MyCons 4 (MyCons 5 (MyCons 6 MyEmpty)))


-- myMap - реализуйте функцию map с использованием типа MyList из материалов лекции

myMap :: (a -> b) -> MyList a -> [b]
myMap f MyEmpty = []
myMap f (MyCons x xs) = (f x) : (myMap f xs)



myLength :: MyList a -> Int
myLength MyEmpty = 0                          -- базовый случай
myLength (MyCons _ xs) = 1 + myLength xs      -- рекурсивный случай


myPrint :: MyList a -> [a]
myPrint MyEmpty = []                          -- базовый случай
myPrint (MyCons x xs) = x : myPrint xs      -- рекурсивный случай


-- myUnFoldr - развертка (операция обратная к свертке)

myUnFoldr :: (b -> Maybe (a, b)) -> b -> [a]

myUnFoldr f b = case f b of
      Nothing -> []
      Just (x, newb) -> x : (myUnFoldr f newb)




-- Расширьте типы для выпекания тортов из материалов лекции:
    -- Добавить возможность испечь не менее трех типов тортов
    -- Контроль числа и объема используемых ингредиентов
    -- Обработку недостатка или отсутствия ингредиентов



