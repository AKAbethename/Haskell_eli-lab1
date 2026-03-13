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



-- myZipSave - попарное объединение двух списков в список пар и сохранение хвоста более длинного списка

helpZipSave :: [a] -> [b] -> ([(a, b)], ([a], [b])) -> ([(a, b)], ([a], [b]))

helpZipSave (x : xs) (y : ys) ans = helpZipSave xs ys ((x, y) : fst ans, snd ans)
helpZipSave [] (y:ys) ans = helpZipSave [] ys (fst ans, (fst (snd ans), y : (snd (snd ans))))
helpZipSave (x:xs) [] ans = helpZipSave xs [] (fst ans, (x : (fst (snd ans)), snd (snd ans)))
helpZipSave [] [] ans = (reverse $ fst ans, (reverse $ fst $ snd ans, reverse $ snd $ snd ans))




myZIP :: [a] -> [b] -> [(a, b)] 

myZIP [] [] = []
myZIP (x:xs) [] = []
myZIP [] (y:ys) = [] 
myZIP (x:xs) (y:ys) = (x, y) : (myZIP xs ys)



{-
myZip :: [a] -> [b] -> ([(a, b)], Either [a] [b])
myZip [] [] = ([], Right [])
myZip [] list = ([], Right list)
myZip list [] = ([], Left list)
myZip (x:xs) (y:ys) = ( (x, y) : (fst (myZip xs ys)), snd (myZip xs ys) )

-}


myZip :: [a] -> [b] -> ([(a, b)], Either [a] [b])
myZip [] [] = ([], Right [])
myZip [] list = ([], Right list)
myZip list [] = ([], Left list)
myZip (x:xs) (y:ys) = let (pairs, rest) = myZip xs ys in ((x, y) : pairs, rest)

-- myUnzipSave - разделение списка пар на пару списков с восстановлением более длинного списка если исходные списки были разного размера

{-
myUnZip :: ([(a, b)], Either [a] [b]) -> ([a], [b])
myUnZip ([], Left []) = ([], [])
myUnZip ([], Right []) = ([], [])
myUnZip (p:ps, Left []) = ( (fst p) : fst (myUnZip (ps, Left [])) , (snd p) : snd (myUnZip (ps, Left [])) )
myUnZip (p:ps, Right []) = ( (fst p) : fst (myUnZip (ps, Right [])) , (snd p) : snd (myUnZip (ps, Right [])) )
myUnZip ([], Left list) = (fst (myUnZip ([], Left [])) ++ list, snd (myUnZip ([], Left [])))
myUnZip ([], Right list) = (fst (myUnZip ([], Right [])), snd (myUnZip ([], Right [])) ++ list)
myUnZip (p:ps, Left list) = ( (fst p) : fst (myUnZip (ps, Left list)) , (snd p) : snd (myUnZip (ps, Left list)) )
myUnZip (p:ps, Right list) = ( (fst p) : fst (myUnZip (ps, Right list)) , (snd p) : snd (myUnZip (ps, Right list)) )
-}

myUnZip :: ([(a, b)], Either [a] [b]) -> ([a], [b])
myUnZip ([], Left []) = ([], [])
myUnZip ([], Right []) = ([], [])
myUnZip (p:ps, Left []) = let (list1, list2) = myUnZip (ps, Left []) in ((fst p) : list1, (snd p) : list2)
myUnZip (p:ps, Right []) = let (list1, list2) = myUnZip (ps, Right []) in ((fst p) : list1, (snd p) : list2)
myUnZip ([], Left list) = let (list1, list2) = myUnZip ([], Left []) in (list1 ++ list, list2)
myUnZip ([], Right list) = let (list1, list2) = myUnZip ([], Right []) in (list1, list2 ++ list)
myUnZip (p:ps, Left list) = let (list1, list2) = myUnZip (ps, Left list) in ((fst p) : list1, (snd p) : list2)
myUnZip (p:ps, Right list) = let (list1, list2) = myUnZip (ps, Right list) in ((fst p) : list1, (snd p) : list2)

myZipSave :: [a] -> [b] -> Either ([(a, b)], [a]) ([(a, b)], [b])
myZipSave xs ys = local xs ys ([], [])
  where
    local [] [] (pairs, rest) = Left (reverse pairs, rest)
    local (x:xs) [] (pairs, rest) = Left (reverse pairs, x:xs)
    local [] (y:ys) (pairs, rest) = Right (reverse pairs, y:ys)
    local (x:xs) (y:ys) (pairs, rest) = local xs ys ((x,y):pairs, rest)

--myZipSave :: [a] -> [b] -> ([([a], [b])], [a])


-- myUnzipSave - разделение списка пар на пару списков с восстановлением более длинного списка если исходные списки были разного размера

-- НЕ РАБОТАЕТ
myUnzipSave2 :: Either ([(a, b)], [a]) ([(a, b)], [b]) -> ([a], [b])
myUnzipSave2 (Left (pairs, extras)) = localLeft (pairs, extras) ([], [])
      where localLeft ([], e) (fans, sans) = (reverse fans ++ e, reverse sans)
            localLeft ((x:xs), e) (fans, sans) = localLeft (xs, e) ((fst x):fans, (snd x):sans)
            
myUnzipSave2 (Right (pairs, extras)) = localRight (pairs, extras) ([], [])
      where localRight ([], e) (fans, sans) = (reverse fans, reverse sans ++ e)
            localRight ((x:xs), e) (fans, sans) = localRight (xs, e) ((fst x):fans, (snd x):sans)

                    


myUnzipSave :: ([(a, a)    ], [a ]     ) -> ([a], [a])
myUnzipSave    ((   z  : zs), (r : rs))  = localfunc2 ((z : zs), (r : rs)) ans 
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

{-
myFoldl1 :: (a -> a -> a) -> [a] -> Maybe a
myFoldl1 _ [] = Nothing
myFoldl1 f (x:xs) = Just (foldl f x xs)
-}

{-
myFoldl1 :: (a -> a -> a) -> [a] -> a
myFoldl1 f (x : xs) = if length (x:xs) > 1 
                        then (x `f` (myFoldl1 f xs)) 
                        else if length (x:xs) == 1
                              then x
                              else undefined -- error "Nothing"
-}

{-
myFoldl1 :: (a -> a -> a) -> [a] -> a
myFoldl1 f (x : xs) = if length (x:xs) > 1 
                        then ((myFoldl1 f xs) `f` x) 
                        else if length (x:xs) == 1
                              then x
                              else undefined -- error "Nothing"

-}



myFoldl1 :: (a -> a -> a) -> [a] -> a
myFoldl1 f (x:xs) = let y:ys = reverse (x:xs) in 
                  if length (y:ys) > 1
                        then (myFoldl1 f (reverse ys)) `f` y
                        else if length (y:ys) == 1
                              then y
                              else undefined

myFoldr1 :: (a -> a -> a) -> [a] -> a
myFoldr1 f (x : xs) = if length (x:xs) > 1 
                        then x `f` (myFoldr1 f xs)
                        else if length (x:xs) == 1
                              then x
                              else undefined -- error "Nothing"






-- myFoldr1 - правая свертка для не пустых списков (без инициирующего значения)
{-
myFoldr1 :: (a -> a -> a) -> [a] -> Maybe a
myFoldr1 _ [] = Nothing
myFoldr1 f (x:xs) = Just (foldr f x xs)
-}

{-
myFoldr1 :: (a -> a -> a) -> [a] -> a
myFoldr1 f (x : xs) = if length (x:xs) > 1 
                        then (x `f` (myFoldl1 f xs)) 
                        else if length (x:xs) == 1
                              then x
                              else undefined -- error "Nothing"

-}


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
myLength MyEmpty = 0                          
myLength (MyCons _ xs) = 1 + myLength xs      


myPrint :: MyList a -> [a]
myPrint MyEmpty = []                         
myPrint (MyCons x xs) = x : myPrint xs      


-- myUnFoldr - развертка (операция обратная к свертке)

myUnFoldr :: (b -> Maybe (a, b)) -> b -> [a]

myUnFoldr f b = case f b of
      Nothing -> []
      Just (x, newb) -> x : (myUnFoldr f newb)


{-

{-
Реализуем пример приготовления торта из материалов прошлой лекции:

> (Масло-шоколадная смесь) состоит из растопленных на слабом огне масла и шоколада
> [Тесто для торта] состоит из 8 взбитых яиц, муки, сахара и разрыхлителя
> {Тесто для шоколадного торта} – это [тесто для торта], перемешанное с (масло-шоколадной смесью)
> Шоколадный торт – это {тесто для шоколадного торта}, выпеченное в духовке при 200C в течение 25 минут.
-}

-- Типы описания составляющих:
data Ingredients = Oil | Chocolate | Egg | Flour | Shugar | BakingPowder deriving Show
data FillingMix = OilChocolateMix deriving Show
data Dough = CakeDough deriving Show
data CakeDough = ChocolateCakeDough deriving Show
data Cake = ChocolateCake deriving Show
data Action = Bake deriving Show

-- Функции, которые описывают процесс приготовления частей торта

{-
makeCakeMix :: Ingredients -> Ingredients -> FillingMix
makeCakeMix Oil Chocolate = OilChocolateMix
makeCakeMix Chocolate Oil = OilChocolateMix
-}

makeCakeMix :: Ingredients -> Ingredients -> Maybe FillingMix
makeCakeMix Oil x = case x of
                        Chocolate -> Just OilChocolateMix
                        _ -> Nothing

-- ...

cakeDough :: Ingredients -> Ingredients -> Ingredients -> Ingredients -> Dough
cakeDough Egg Flour Shugar BakingPowder = CakeDough
-- ...

chocolateCakeDough :: Dough -> FillingMix -> CakeDough
chocolateCakeDough CakeDough OilChocolateMix = ChocolateCakeDough
-- ...

chocolateCake :: CakeDough -> Action -> Cake
chocolateCake ChocolateCakeDough Bake = ChocolateCake
-- ...

-- Промежуточные стадии приготовления торта:
myDough = cakeDough Egg Flour Shugar BakingPowder
notMyDough = cakeDough Egg Egg Egg Egg -- ! не работает
myMix = makeCakeMix Oil Chocolate
--myCakeDough = chocolateCakeDough myDough myMix
myCakeDough = case makeCakeMix Oil Chocolate of
    Just mix -> chocolateCakeDough myDough mix
    Nothing -> error "Не удалось сделать смесь!"
-- Финальный торт:
myCake = chocolateCake myCakeDough Bake

{-
Типы можно расширить и параметризовать для отслеживания объема и количества ингредиентов
-}

-}


-- Расширьте типы для выпекания тортов из материалов лекции:
    -- Добавить возможность испечь не менее трех типов тортов
    -- Контроль числа и объема используемых ингредиентов
    -- Обработку недостатка или отсутствия ингредиентов



data Cake = ChocolateCake | HoneyCake | NegroSmileCake deriving Show
data Ingredients = Oil | Chocolate | Egg | Flour | Shugar | BakingPowder 
                       | Sault | Soda | StrawberryJam | Kefir
                       | BlackcurrantJam 
                       | Honey | Butter | Vanilin deriving Show

data FillingMix = OilChocolateMix | EggShugarVanilinBlackcurrantJamMix | HoneyShugarButterMix deriving Show

data CakeDough = ChocolateCakeDough | HoneyCakeDough | NegroSmileCakeDough deriving Show

data Action = Bake | Knead deriving Show


controlEggs :: Ingredients -> Int  -- 2
controlEggs Egg = 2

controllHoney :: Ingredients -> Int  -- 50g
controllHoney Honey = 50

massHoney = controllHoney Honey

cntEggs = controlEggs Egg

makeCakeMix :: Ingredients -> Ingredients -> Maybe FillingMix
makeCakeMix x Oil = case x of
                  Chocolate -> Just OilChocolateMix
                  _ -> Nothing


makeCakeMixH :: (Ingredients, Int) -> Ingredients -> Ingredients -> Maybe FillingMix
makeCakeMixH (Honey, x) Shugar Butter 
                        | x == (controllHoney Honey) = Just HoneyShugarButterMix
                        | otherwise = Nothing
makeCakeMixH _ _ _ = Nothing


makeCakeMixN :: (Ingredients, Int) -> Ingredients -> Ingredients -> Ingredients ->  Maybe FillingMix
makeCakeMixN (Egg, x) Shugar Vanilin BlackcurrantJam 
                                          | x == (controlEggs Egg) = Just EggShugarVanilinBlackcurrantJamMix
                                          | otherwise = Nothing
makeCakeMixN _ _ _ _ = Nothing


honeyCakeDough :: Ingredients -> Maybe FillingMix -> Maybe CakeDough
honeyCakeDough Flour x = case x of
                  Just HoneyShugarButterMix -> Just HoneyCakeDough
                  Nothing -> Nothing


negroSmileCakeDough :: Ingredients -> Maybe FillingMix -> Maybe CakeDough
negroSmileCakeDough Flour x = case x of
                  Just EggShugarVanilinBlackcurrantJamMix -> Just NegroSmileCakeDough
                  Nothing -> Nothing

honeyCake :: Maybe CakeDough -> Action -> Action -> Maybe Cake
honeyCake x Knead Bake = case x of
                        Just HoneyCakeDough -> Just HoneyCake
                        Nothing -> Nothing


negroSmileCake :: Maybe CakeDough -> Action -> Action -> Maybe Cake
negroSmileCake x Knead Bake = case x of
                        Just NegroSmileCakeDough -> Just NegroSmileCake
                        Nothing -> Nothing

{-
data FillingMix = OilChocolateMix deriving Show
data Dough = CakeDough deriving Show
data CakeDough = ChocolateCakeDough deriving Show
data Cake = ChocolateCake deriving Show
data Action = Bake deriving Show
-}


-- Функции, которые описывают процесс приготовления частей торта

{-
makeCakeMix :: Ingredients -> Ingredients -> FillingMix
makeCakeMix Oil Chocolate = OilChocolateMix
makeCakeMix Chocolate Oil = OilChocolateMix
-}

{-

makeCakeMix :: Ingredients -> Ingredients -> Maybe FillingMix
makeCakeMix Oil x = case x of
                        Chocolate -> Just OilChocolateMix
                        _ -> Nothing

-- ...

cakeDough :: Ingredients -> Ingredients -> Ingredients -> Ingredients -> Dough
cakeDough Egg Flour Shugar BakingPowder = CakeDough
-- ...

chocolateCakeDough :: Dough -> FillingMix -> CakeDough
chocolateCakeDough CakeDough OilChocolateMix = ChocolateCakeDough
-- ...

chocolateCake :: CakeDough -> Action -> Cake
chocolateCake ChocolateCakeDough Bake = ChocolateCake
-- ...

-- Промежуточные стадии приготовления торта:
myDough = cakeDough Egg Flour Shugar BakingPowder
notMyDough = cakeDough Egg Egg Egg Egg -- ! не работает
myMix = makeCakeMix Oil Chocolate
--myCakeDough = chocolateCakeDough myDough myMix
myCakeDough = case makeCakeMix Oil Chocolate of
    Just mix -> chocolateCakeDough myDough mix
    Nothing -> error "Не удалось сделать смесь!"
-- Финальный торт:
myCake = chocolateCake myCakeDough Bake

-}

