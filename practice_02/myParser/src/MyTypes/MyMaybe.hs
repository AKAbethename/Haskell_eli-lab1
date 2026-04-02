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
    (<>) MyNothing b = b
    (<>) a MyNothing = a
    (<>) (MyJust x) (MyJust y) = MyJust ((<>) x y)


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
- (<>), sconcat, stimes   -----
- mappend, mconcat    ------
- fmap, (<$)    -----
- pure, (<*>), liftA2, (*>), (<*)   -------

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

