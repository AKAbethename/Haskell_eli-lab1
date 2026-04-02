module MyMaybeMod where

import Data.Semigroup

data MyMaybe a = MyNothing | MyJust a deriving (Show, Eq)

-- тип MyMaybe, аналогичный стандартному Maybe. Сделайте этот тип представителем классов типов
-- Foldable, Semigroup, Monoid, Functor и Applicative (*)

instance Foldable MyMaybe where
    {- # MINIMAL foldMap | foldr # -}

    foldr f x MyNothing = x
    foldr f x (MyJust a) = foldr f (f a x) Nothing


instance Functor MyMaybe where
    -- minimal - fmap

    fmap _ MyNothing = MyNothing
    fmap f (MyJust a) = MyJust (f a)


instance Applicative MyMaybe where
    -- minimal - pure && <*>

    pure a = (MyJust a)

    (<*>) MyNothing _ = MyNothing
    (<*>) (MyJust f) MyNothing = MyNothing
    (<*>) (MyJust f) (MyJust a) = MyJust (f a)


instance Semigroup a => Semigroup (MyMaybe a) where
    -- minimal - (<>)
    MyNothing <> b = b
    a <> MyNothing = a
    (MyJust x) <> (MyJust y) = MyJust (x <> y)


instance Semigroup a => Monoid (MyMaybe a) where
    -- mempty or mconcat
    mempty = MyNothing



data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown | Alpha deriving (Show, Eq)

instance Semigroup Color where
  (<>) Red    Blue    = Purple
  (<>) Blue   Red     = Purple
  (<>) Yellow Blue    = Green
  (<>) Blue   Yellow  = Green
  (<>) Yellow Red     = Orange
  (<>) Red    Yellow  = Orange

  (<>) Red    Alpha   = Red 
  (<>) Yellow Alpha   = Yellow 
  (<>) Blue   Alpha   = Blue 
  (<>) Green  Alpha   = Green 
  (<>) Purple Alpha   = Purple 
  (<>) Orange Alpha   = Orange 
  (<>) Brown  Alpha   = Brown 
  (<>) Alpha  Red     = Red 
  (<>) Alpha  Yellow  = Yellow 
  (<>) Alpha  Blue    = Blue 
  (<>) Alpha  Green   = Green 
  (<>) Alpha  Purple  = Purple 
  (<>) Alpha  Orange  = Orange 
  (<>) Alpha  Brown   = Brown 



-- Многострочный комментарий
{-

- fold, foldMap, foldr   ------ DONE
- (<>), sconcat, stimes   -----  Done
- mappend, mconcat    ------  DONE
- fmap, (<$)    -----  DONE
- pure, (<*>), liftA2, (*>), (<*)   ------- DONE

-}


-- fold
--ghci> t
--MyJust 4
--ghci> foldr (\x y -> x * y) 1 t
--4
--ghci> foldr (\x y -> x * y) 2 t
--8

-- foldMap
--ghci> t = MyNothing
--ghci> foldMap (\x -> show x) t
--""
--ghci> t = MyJust 10
--ghci> foldMap (\x -> show x) t
--"10"


-- foldr
--ghci> t
--MyJust 10
--ghci> foldr (\x y -> x + y) 2 t
--12


-- (<>)

--ghci> MyJust "Hello" <> MyJust " World"
--MyJust "Hello World"


-- stimes

--ghci> stimes 3 (MyJust "eli")
--MyJust "elielieli"


-- sconcat

--ghci> import Data.List.NonEmpty
--ghci> objs = MyJust "Hello" :| [MyNothing, MyJust " World"]
--ghci> sconcat objs
--MyJust "Hello World"



-- mappend

--ghci>  MyJust "Hello" `mappend` MyNothing
--MyJust "Hello"


-- mconcat

--ghci> mconcat [MyJust "ha", MyJust " eli"]
--MyJust "ha eli"



-- fmap

--ghci> fmap (\x -> x + 10) (MyJust 2)
--MyJust 12


-- (<$)

--ghci> (<$) 505 MyJust 12
--505


-- pure and (<*>)

--ghci> (pure (\x -> x + 10)) <*> (MyJust 2)
--MyJust 12


-- liftA2

--ghci> liftA2 (\x y -> x + y + 1) (MyJust 10) (MyJust 20)
--MyJust 31


-- (<*)

--ghci> (<*) (MyJust 10) (MyJust 20)
--MyJust 10


-- (*>)

--ghci> (*>) (MyJust 10) (MyJust 20)
--MyJust 20







