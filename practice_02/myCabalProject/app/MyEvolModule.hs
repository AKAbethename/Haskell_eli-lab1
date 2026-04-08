module MyEvolModule where

-- import qualified Data.Random as R

{-
data MyEvolution = LUCA "Last Universal Common Ancestor" | Cyanobacteria "Synechococcus" | Trilobite "Paradoxides" | 
                 Ichthyostega "Ichthyostega" | Dimetrodon "Dimetrodon" | Archaeopteryx "Archaeopteryx" | Morganucodon "Morganucodon" |
                Purgatorius "Purgatorius" | Australopithecine "Australopithecus Afarensis" | Humans "Homo Sapiens" deriving Show
-}

data MyEvolution = LUCA | Cyanobacteria | Trilobite | Ichthyostega | Dimetrodon | Archaeopteryx | Morganucodon | Purgatorius | Australopithecine | Humans 


instance Show MyEvolution where
    show LUCA = "Last Universal Common Ancestor"
    show Cyanobacteria = "Synechococcus"
    show Trilobite = "Paradoxides"
    show Ichthyostega = "Ichthyostega"
    show Dimetrodon = "Dimetrodon"
    show Archaeopteryx = "Archaeopteryx"
    show Morganucodon = "Morganucodon"
    show Purgatorius = "Purgatorius"
    show Australopithecine = "Australopithecus Afarensis"
    show Humans = "Homo Sapiens"



instance Read MyEvolution where
    readsPrec _ str = case str of 
        'L' : 'a' : 's' : 't' : ' ' :
         'U' : 'n' : 'i' : 'v' : 'e' : 'r' : 's' : 'a' : 'l' : ' ' 
         : 'C' : 'o' : 'm' : 'm' : 'o' : 'n' : ' ' 
         : 'A' : 'n' : 'c' : 'e' : 's' : 't' : 'o' : 'r' : rest -> [(LUCA, rest)]
        
        'S' : 'y' : 'n' : 'e' : 'c' : 'h' : 'o' : 'c' : 'o' : 'c' : 'c' : 'u' : 's' : rest -> [(Cyanobacteria, rest)]

        'P' : 'a' : 'r' : 'a' : 'd' : 'o' : 'x' : 'i' : 'd' : 'e' : 's' : rest -> [(Trilobite, rest)]
        
        'I' : 'c' : 'h' : 't' : 'h' : 'y' : 'o' : 's' : 't' : 'e' : 'g' : 'a' : rest -> [(Ichthyostega, rest)]

        'D' : 'i' : 'm' : 'e' : 't' : 'r' : 'o' : 'd' : 'o' : 'n' : rest  -> [(Dimetrodon, rest)]
    
        'A' : 'r' : 'c' : 'h' : 'a' : 'e' : 'o' : 'p' : 't' : 'e' : 'r' : 'y' : 'x' : rest  -> [(Archaeopteryx, rest)]
    
        'M' : 'o' : 'r' : 'g' : 'a' : 'n' : 'u' : 'c' : 'o' : 'd' : 'o' : 'n' : rest -> [(Morganucodon, rest)]
    
        'P' : 'u' : 'r' : 'g' : 'a' : 't' : 'o' : 'r' : 'i' : 'u' : 's' : rest  -> [(Purgatorius, rest)]
    
        'A' : 'u' : 's' : 't' : 'r' : 'a' : 'l' : 'o' : 'p' : 'i' : 't' : 'h' : 'e' : 'c' : 'u' : 's' : ' ' :
                  'A' : 'f' : 'a' : 'r' : 'e' : 'n' : 's' : 'i' : 's' : rest  -> [(Australopithecine, rest)]
    
        'H' : 'o' : 'm' : 'o' : ' ' : 'S' : 'a' : 'p' : 'i' : 'e' : 'n' : 's' : rest  -> [(Humans, rest)]

        

--   readsPrec _ "LUCA" = [(LUCA, "")]


-- readsPrec :: Int -> ReadS a

-- type ReadS a = String -> [(a, String)]


instance Eq MyEvolution where
    (==) LUCA LUCA = True
    (==) Cyanobacteria Cyanobacteria = True
    (==) Trilobite Trilobite = True
    (==) Ichthyostega Ichthyostega = True
    (==) Dimetrodon Dimetrodon = True
    (==) Archaeopteryx Archaeopteryx = True
    (==) Morganucodon Morganucodon = True
    (==) Purgatorius Purgatorius = True
    (==) Australopithecine Australopithecine = True
    (==) Humans Humans = True
    (==) _ _ = False
    (/=) x y = not (x == y)


instance Ord MyEvolution where
   
    (<=) LUCA _ = True
    (<=) Cyanobacteria LUCA = False
    (<=) Cyanobacteria _ = True

    (<=) Trilobite LUCA = False
    (<=) Trilobite Cyanobacteria = False
    (<=) Trilobite _ = True

    (<=) Ichthyostega LUCA = False
    (<=) Ichthyostega Cyanobacteria = False
    (<=) Ichthyostega Trilobite = False
    (<=) Ichthyostega _ = True

    (<=) Dimetrodon LUCA = False
    (<=) Dimetrodon Cyanobacteria = False
    (<=) Dimetrodon Trilobite = False
    (<=) Dimetrodon Ichthyostega = False
    (<=) Dimetrodon _ = True

    (<=) Archaeopteryx LUCA = False
    (<=) Archaeopteryx Cyanobacteria = False
    (<=) Archaeopteryx Trilobite = False
    (<=) Archaeopteryx Ichthyostega = False
    (<=) Archaeopteryx Dimetrodon = False
    (<=) Archaeopteryx _ = True

    (<=) Morganucodon LUCA = False
    (<=) Morganucodon Cyanobacteria = False
    (<=) Morganucodon Trilobite = False
    (<=) Morganucodon Ichthyostega = False
    (<=) Morganucodon Dimetrodon = False
    (<=) Morganucodon Archaeopteryx = False
    (<=) Morganucodon _ = True

    (<=) Purgatorius LUCA = False
    (<=) Purgatorius Cyanobacteria = False
    (<=) Purgatorius Trilobite = False
    (<=) Purgatorius Ichthyostega = False
    (<=) Purgatorius Dimetrodon = False
    (<=) Purgatorius Archaeopteryx = False
    (<=) Purgatorius Morganucodon = False
    (<=) Purgatorius _ = True

    (<=) Australopithecine LUCA = False
    (<=) Australopithecine Cyanobacteria = False
    (<=) Australopithecine Trilobite = False
    (<=) Australopithecine Ichthyostega = False
    (<=) Australopithecine Dimetrodon = False
    (<=) Australopithecine Archaeopteryx = False
    (<=) Australopithecine Morganucodon = False
    (<=) Australopithecine Purgatorius = False
    (<=) Australopithecine _ = True

    (<=) Humans LUCA = False
    (<=) Humans Cyanobacteria = False
    (<=) Humans Trilobite = False
    (<=) Humans Ichthyostega = False
    (<=) Humans Dimetrodon = False
    (<=) Humans Archaeopteryx = False
    (<=) Humans Morganucodon = False
    (<=) Humans Purgatorius = False
    (<=) Humans Australopithecine = False
    (<=) Humans _ = True

    compare x y = if (x == y) then EQ
                    else if (x <= y) then LT
                    else GT

    (<) x y = case compare x y of
        LT -> True
        _ -> False
--    (<=) x y = case compare x y of
--        GT -> False
--        _ -> True
    (>) x y = case compare x y of
        GT -> True
        _ -> False
    (>=) x y = case compare x y of
        LT -> False
        _ -> True
    min x y = if x <= y then x else y
    max x y = if x <= y then y else x

{-
x < y = case compare x y of {LT -> True; _ -> False}
x <= y = case compare x y of {GT -> False; _ -> True}
x > y = case compare x y of {GT -> True; _ -> False}
x >= y = case compare x y of {LT -> False; _ -> True}
max x y = if x <= y then y else x
min x y = if x <= y then x else y
-}

instance Enum MyEvolution where
    toEnum 0 = LUCA
    toEnum 1 = Cyanobacteria
    toEnum 2 = Trilobite
    toEnum 3 = Ichthyostega
    toEnum 4 = Dimetrodon
    toEnum 5 = Archaeopteryx
    toEnum 6 = Morganucodon 
    toEnum 7 = Purgatorius
    toEnum 8 = Australopithecine
    toEnum 9 = Humans
    fromEnum LUCA = 0
    fromEnum Cyanobacteria = 1
    fromEnum Trilobite = 2
    fromEnum Ichthyostega = 3
    fromEnum Dimetrodon = 4
    fromEnum Archaeopteryx = 5
    fromEnum Morganucodon = 6
    fromEnum Purgatorius = 7
    fromEnum Australopithecine = 8
    fromEnum Humans = 9

instance Bounded MyEvolution where
    minBound = LUCA
    maxBound = Humans


{- MyEvolution' -}

data MyEvolution' = LUCA' | Cyanobacteria' | Trilobite' | Ichthyostega' | Dimetrodon' | Archaeopteryx' | Morganucodon' | Purgatorius' | Australopithecine' | Humans' deriving (Show, Read, Eq, Ord, Enum, Bounded)

{-


2. Задайте тип-сумму MyEvolution в MyEvolModule.hs со следующими конструкторами и соответствующими строками (для класса типов Show):

Конструктор данных  | Соответствующая строка для классов типов Show
--------------------+---------------------------------------------- 
LUCA                | "Last Universal Common Ancestor"
Cyanobacteria       | "Synechococcus"
Trilobite           | "Paradoxides"
Ichthyostega        | "Ichthyostega"
Dimetrodon          | "Dimetrodon"
Archaeopteryx       | "Archaeopteryx"
Morganucodon        | "Morganucodon"
Purgatorius         | "Purgatorius"
Australopithecine   | "Australopithecus Afarensis"
Humans              | "Homo Sapiens"

Напишите вручную представителей классов типов: Show, Read, Eq, Ord, Enum, Bounded
Напишите аналогичный тип-сумму MyEvolution' и используя механизм deriving сделайте его представителем классов типов: Show, Read, Eq, Ord, Enum, Bounded

Добавьте в функцию main файла Main.hs вывод отсортированного списка значений типа MyEvolution. Используйте следующий код: putStrLn $ show $ sort ([...] :: [MyEvolution])

-}


