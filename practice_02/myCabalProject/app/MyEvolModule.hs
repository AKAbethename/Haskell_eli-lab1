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
    readsPrec

-- readsPrec :: Int -> ReadS a


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