# Практическое задание №2. Пользовательские типы. Тип как экземпляр класса. Парсеры.


## Задание
### Часть 1.
1. Создайте и настройте проект MyEvolProject в Cabal
Добавьте дополнительную зависимость в проект: random
Создайте новый модуль MyEvolModule.hs в папке app.

2.Задайте тип-сумму MyEvolution в MyEvolModule.hs со следующими конструкторами и соответствующими строками (для класса типов Show):

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

### Часть 2.


1. Создайте и настройте stack проект myParser

2. Создайте в src директорию MyTypes и создайте в нем три модуля со следующими реализациями:
- тип MyTree, который содержит значения в узлах и листьях. Сделайте этот тип представителем классов типов Foldable, Functor и Applicative (*)
- тип MyMaybe, аналогичный стандартному Maybe. Сделайте этот тип представителем классов типов Foldable, Semigroup, Monoid, Functor и Applicative (*)
- тип MyEither, аналогичный стандартному Either. Сделайте этот тип представителем классов типов Foldable, Semigroup, Functor и Applicative (*)
(*) для других необходимых классов типов можно использовать механизм deriving

Добавьте в конец каждого файла многострочный комментарий: 
для реализованного типа и определенных представителей классов типов напишите пример вызова соответствующих функций и результатов работы из ghci 
(если класс типов не требуется определять по заданию, то пример вызова не нужен):
- fold, foldMap, foldr
- (<>), sconcat, stimes
- mappend, mconcat
- fmap, (<$)
- pure, (<*>), liftA2, (*>), (<*)

3. Импортируйте модули в Main.hs



### Часть 3.

Расширьте проект myParser созданный в практическом задании 2 части 2 
- создайте в src директорию MyParsers для файлов этого задания
- скопируйте реализацию парсера из лекции в новый модуль. Модифицируйте разработанный на лекции парсер заменив стандартный тип Maybe на собственный тип MyMaybe из 2-ой части 2-го задания
- создайте два новых модуля и в каждом из них реализуйте функционал повторяющий лекционный материал, но с использованием библиотек Parsec и Attoparsec
- изолируйте решения друг от друга и вызовите их через точку входа в проекте используя следующий код (с точностью до квалифицированного импорта):

main :: IO ()
main = do
    putStrLn "MyParser:"
    putStrLn $ show (runParser plusOrMult "12*345dsf")
    putStrLn $ show (runParser plusOrMult "12+345dsf")
    putStrLn "Parsec:"
    putStrLn $ show (runParser plusOrMultParsec "12*345dsf")
    putStrLn $ show (runParser plusOrMultParsec "12+345dsf")
    putStrLn "Attoparsec:"
    putStrLn $ show (runParser plusOrMultAttoparsec "12*345dsf")
    putStrLn $ show (runParser plusOrMultAttoparsec "12+345dsf")

-}


## Основные возможности

1.Создан модуль MyEvolModule, в котором создан пользовательский тип данных MyEvolution. Далее MyEvolution делается экземпляром классов Show, Read, Ord, Eq, Enum и Bounded, без использования ключевого слова deriving.

2.Созданы модули MyTree, MyMaybe и MyEither, являющиеся пользовательскими реализациями бинарного дерева, монады Maybe и Either. Созданные типы делаются экземплярами классов Functor, Foldable, Applicative, Semigroup, Monoid.

3. Создан модуль ParserEli, повторяющий лекционный материал. В нем реализован парсер простых ошибок. В отличие от лекционного материала, в данном модуле использование Maybe заменяется использованием MyMaybe. Так же созданы модули ParsecEli и AttoparsecEli, повторяющие функционал из лекционного материала, но использующие библиотеки Parsec и Attoparsec. Созданные модули импортированы в модуль Main.

## Использование

Перед созданием каждого модуля была использована система сборки stack. Для этого в командной строке необходимо было прописать команду

```bash
stack build
```

Далее, когда все необходимые библиотеки были загружены, можно запустить проект при помощи команды
```bash
stack run
```

Созданы все необходимые файлы для сборки. Модуль Main находится в директории app, вспомогательные модули находятся в директории src.
