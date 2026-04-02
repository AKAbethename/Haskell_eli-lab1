module MyTreeMod where

--import qualified Data.Functor

import Data.Semigroup

data MyTree a = Node {
        value :: a,       
        subforest :: MyForest a  
} deriving (Show, Eq)  

{- синтаксис записей, rose tree -}

type MyForest a = [MyTree a]



instance Foldable MyTree where
{- # MINIMAL foldMap | foldr # -}
    foldr f z = \t -> go t z  -- Use a lambda to allow inlining with two arguments
      where
        go (Node x ts) = f x . foldr (\t k -> go t . k) id ts


instance Functor MyTree where
    -- minimal -- fmap
--    fmap = fmapTree
    fmap f (Node x fs) = Node (f x) (map (fmap f) fs)



instance Applicative MyTree where
    -- A minimal complete definition must include implementations of pure and of either <*> or liftA2.
    -- If it defines both, then they must behave the same as their default definitions:

    -- minimal - pure, (<*>) 
    pure x = Node x []

    Node f tfs <*> tx@(Node x txs) = Node (f x) (map (f <$>) txs ++ map (<*> tx) tfs)



{-
data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
    deriving Foldable
-}




-- Многострочный комментарий
{-

- fold, foldMap, foldr
- (<>), sconcat, stimes
- mappend, mconcat
- fmap, (<$)
- pure, (<*>), liftA2, (*>), (<*)

-}

--ghci> tree = Node 5 [Node 3 [], Node 2 []]
--ghci> foldl (\x y -> x + y) 0 tree
--10

--foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
--ghci> foldr (\x y -> x + y) 0 tree
--10

--ghci> foldMap (\x -> show x) tree
--"532"

--ghci> tree = Node 5 [Node 4 [], Node 3 []]
--ghci> (<$) 2 tree
--Node {value = 2, subforest = [Node {value = 2, subforest = []},Node {value = 2, subforest = []}]}



--ghci> tree1 = pure 6
--ghci> tree1
--6

--ghci> treef = Node (+5) [Node (+4) [], Node (+3) []]
--ghci> tree = Node 5 [Node 4 [], Node 3 []]
--ghci> treef <*> tree
--Node {value = 10, subforest = [Node {value = 9, subforest = []},
--        Node {value = 8, subforest = []},
--        Node {value = 9, subforest = [Node {value = 8, subforest = []},
--        Node {value = 7, subforest = []}]},
--        Node {value = 8, subforest = [Node {value = 7, subforest = []},
--        Node {value = 6, subforest = []}]}]}


--ghci> tree
--Node {value = 5, subforest = [Node {value = 4, subforest = []},Node {value = 3, subforest = []}]}
--ghci> tree2 = Node 10 [Node 9 [], Node 8 []]
--ghci> liftA2 (\x y -> x ** y) tree tree2
--    Node {value = 9765625.0, subforest = [Node {value = 1953125.0, subforest = []},
--    Node {value = 390625.0, subforest = []},
--    Node {value = 1048576.0, subforest = [Node {value = 262144.0, subforest = []},
--    Node {value = 65536.0, subforest = []}]},
--    Node {value = 59049.0, subforest = [Node {value = 19683.0, subforest = []},
--    Node {value = 6561.0, subforest = []}]}]}



--ghci> tree
--Node {value = 5, subforest = [Node {value = 4, subforest = []},Node {value = 3, subforest = []}]}
--ghci> tree2
--Node {value = 10, subforest = [Node {value = 9, subforest = []},Node {value = 8, subforest = []}]}
--ghci> (*>) tree tree2
--Node {value = 10, subforest = [Node {value = 9, subforest = []},
--    Node {value = 8, subforest = []},
--    Node {value = 10, subforest = [Node {value = 9, subforest = []},
--    Node {value = 8, subforest = []}]},
--    Node {value = 10, subforest = [Node {value = 9, subforest = []},
--    Node {value = 8, subforest = []}]}]}



--ghci> (<*) tree tree2
--Node {value = 5, subforest = [Node {value = 5, subforest = []},
--    Node {value = 5, subforest = []},
--    Node {value = 4, subforest = [Node {value = 4, subforest = []},
--    Node {value = 4, subforest = []}]},
--    Node {value = 3, subforest = [Node {value = 3, subforest = []},
--    Node {value = 3, subforest = []}]}]}


--ghci> tree = Node 5 [Node 3 [], Node 2 []]
--ghci> fmap (*2) tree
--Node {value = 10, subforest = [Node {value = 6, subforest = []},Node {value = 4, subforest = []}]}



