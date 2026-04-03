module MyTypes.MyEither where

data MyEither a b = MyLeft a | MyRight b deriving (Show, Eq)

-- тип MyEither, аналогичный стандартному Either. Сделайте этот тип представителем классов типов
-- Foldable, Semigroup, Functor и Applicative (*)


instance Foldable (MyEither a) where
    -- minimal foldr / foldmap
    foldr _ b (MyLeft x) = b
    foldr f x (MyRight y) = (f y x)


instance Functor (MyEither a) where
    -- minimal - fmap
    fmap _ (MyLeft x) = MyLeft x
    fmap f (MyRight y) = MyRight (f y)


instance Applicative (MyEither a) where
    -- minimal pure && <*>

    pure x = MyRight x

    (<*>) (MyLeft f) _ = MyLeft f
    (<*>) (MyRight f) r = fmap f r

instance Semigroup (MyEither a b) where
    -- minimal <>

{-
    (<>) (MyLeft x) _ = MyLeft x
    (<>) _ (MyLeft x) = MyLeft x
    (<>) (MyRight x) (MyRight y) = MyRight (x)
-}
 --   Left _ <> b = b
 --   a      <> _ = a
    (<>) (MyLeft _) b = b
    (<>) a _ = a



-- тип MyEither, аналогичный стандартному Either. Сделайте этот тип представителем классов типов 
-- Foldable, Semigroup, Functor и Applicative (*)

-- fold, foldMap, foldr   -- DONE
-- (<>), sconcat, stimes  -- DONE
-- fmap, (<$)   -- DONE
-- pure, (<*>), liftA2, (*>), (<*)  -- DONE


-- foldr

-- ghci> foldr (\x y -> x + y) 0 (MyLeft 5)
-- 0
-- ghci> foldr (\x y -> x + y) 0 (MyRight 5)
-- 5


-- foldl

-- ghci> foldl (\x y -> x + y) 0 (MyRight 5)
-- 5
-- ghci> foldl (\x y -> x + y) 0 (MyLeft 5)
-- 0


-- foldMap

-- ghci> foldMap (\x -> show x) (MyLeft 10)
-- ""
-- ghci> foldMap (\x -> show x) (MyRight 10)
-- "10"



-- fmap

-- ghci> fmap (*3) (MyLeft 5)
-- MyLeft 5
-- ghci> fmap (*3) (MyRight 5)
-- MyRight 15


-- (<$)

-- ghci> (<$) 505 (MyLeft 100)
-- MyLeft 100
-- ghci> (<$) 505 (MyRight 100)
-- MyRight 505


-- pure and (<*>)

-- ghci> fmap (*3) (MyRight 5)
-- MyRight 15
-- ghci> (<*>) (pure (\x -> x * 3)) (MyRight 5)
-- MyRight 15
-- ghci> (<*>) (pure (\x -> x * 3)) (MyLeft 5)
-- MyLeft 5



-- liftA2

-- ghci> liftA2 (\x y -> x + y + 2) (MyLeft 5) (MyLeft 3)
-- MyLeft 5
-- ghci> liftA2 (\x y -> x + y + 2) (MyLeft 5) (MyRight 3)
-- MyLeft 5
-- ghci> liftA2 (\x y -> x + y + 2) (MyRight 5) (MyLeft 3)
-- MyLeft 3
-- ghci> liftA2 (\x y -> x + y + 2) (MyRight 5) (MyRight 3)
-- MyRight 10


-- (*>)

-- ghci> (*>) (MyLeft 100) (MyLeft 200)
-- MyLeft 100
-- ghci> (*>) (MyRight 100) (MyLeft 200)
-- MyLeft 200
-- ghci> (*>) (MyRight 100) (MyRight 200)
-- MyRight 200


-- (<*)

-- ghci> (<*) (MyRight 100) (MyRight 200)
-- MyRight 100
-- ghci> (<*) (MyRight 100) (MyLeft 200)
-- MyLeft 200
-- ghci> (<*) (MyLeft 100) (MyRight 200)
-- MyLeft 100
-- ghci> (<*) (MyLeft 100) (MyLeft 200)
-- MyLeft 100



-- (<>)

-- ghci> MyRight 10 <> MyRight 5
-- MyRight 10
-- ghci> MyRight 10 <> MyLeft 5
-- MyRight 10
-- ghci> MyLeft 10 <> MyRight 5
-- MyRight 5
-- ghci> MyLeft 10 <> MyLeft 5
-- MyLeft 5


-- sconcat


-- ghci> objs = MyRight "Hello" :| [MyLeft " ", MyRight "World!"]
-- ghci> sconcat objs
-- MyRight "Hello"
-- ghci> objs = MyLeft "Hello" :| [MyLeft " ", MyRight "World!"]
-- ghci> sconcat objs
-- MyRight "World!"
-- ghci> objs = MyLeft "Hello" :| [MyRight " ", MyRight "World!"]
-- ghci> sconcat objs
-- MyRight " "



-- stimes

-- ghci> stimes 5 (MyRight "Eli")
-- MyRight "Eli"
-- ghci> stimes 5 (MyLeft "Eli")
-- MyLeft "Eli"



