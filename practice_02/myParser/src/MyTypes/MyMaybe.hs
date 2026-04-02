module MyMaybeMod where

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