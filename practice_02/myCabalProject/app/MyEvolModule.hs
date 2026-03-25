--import qualified Data.Random as R

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
         : 'A' : 'n' : 'c' : 'e' : 's' : 't' : 'o' : 'r' : rest -> [(LUCA, "")]
        
        'S' : 'y' : 'n' : 'e' : 'c' : 'h' : 'o' : 'c' : 'o' : 'c' : 'c' : 'u' : 's' : rest -> [(Cyanobacteria, "")]

        'P' : 'a' : 'r' : 'a' : 'd' : 'o' : 'x' : 'i' : 'd' : 'e' : 's' : rest -> [(Trilobite, "")]
        
        'I' : 'c' : 'h' : 't' : 'h' : 'y' : 'o' : 's' : 't' : 'e' : 'g' : 'a' : rest -> [(Ichthyostega, "")]

        'D' : 'i' : 'm' : 'e' : 't' : 'r' : 'o' : 'd' : 'o' : 'n' : rest  -> [(Dimetrodon, rest)]
    
        'r' : 'c' : 'h' : 'a' : 'e' : 'o' : 'p' : 't' : 'e' : 'r' : 'y' : 'x' : rest  -> [(Archaeopteryx, rest)]
    
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




instance Enum SixSidedDie' where
    toEnum 0 = S1'
    toEnum 1 = S2'
    toEnum 2 = S3'
    toEnum 3 = S4'
    toEnum 4 = S5'
    toEnum 5 = S6'
    fromEnum S1' = 0
    fromEnum S2' = 1
    fromEnum S3' = 2
    fromEnum S4' = 3
    fromEnum S5' = 4
    fromEnum S6' = 5



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


data SixSidedDie' = S1' | S2' | S3' | S4' | S5' | S6'

{-
Класс типов Show
Преобразует значение в строку и может вывести ее на экран
:i Show
...
instance Show SixSidedDie' -- Defined at lec_05.hs:290:10
...
Представитель Show:
-}
instance Show SixSidedDie' where
    show S1' = "One"
    show S2' = "Two"
    show S3' = "Three"
    show S4' = "Four"
    show S5' = "Five"
    show S6' = "Six"

myDie = S6'
-- myDie

{-
Класс типов Read
Обратное преобразование строк в значения
-}
instance Read SixSidedDie' where
    readsPrec _ str = case str of
        'r':'o':'l':'l':' ':'o':'n':'e':rest          -> [(S1',  rest)]
        'r':'o':'l':'l':' ':'t':'w':'o':rest          -> [(S2',  rest)]
        'r':'o':'l':'l':' ':'t':'h':'r':'e':'e':rest  -> [(S3',  rest)]
        'r':'o':'l':'l':' ':'f':'o':'u':'r':rest      -> [(S4',  rest)]
        'r':'o':'l':'l':' ':'f':'i':'v':'e':rest      -> [(S5',  rest)]
        'r':'o':'l':'l':' ':'s':'i':'x':rest          -> [(S6',  rest)]




-- (read "roll two")
-- (read "roll two") :: SixSidedDie'
