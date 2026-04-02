module MyEitherMod where

data MyEither a b = MyLeft a | MyRight b

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
