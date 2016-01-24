data BiTree a = EmptyTree | BiTree a (BiTree a) (BiTree a) deriving(Show, Read, Eq)

data Maybe1 a = Just1 a | Nothing1 deriving(Show, Read, Eq)

instance Functor Maybe1 where
	fmap f (Just1 x) = Just1 (f x)
	fmap f Nothing1 = Nothing1

div2 :: Int -> Int
div2 x = div x 2

treeVal :: BiTree a -> Maybe1 a
treeVal EmptyTree = Nothing1
treeVal (BiTree x t1 t2) = Just1 x

biTreeAdd ::(Ord a) => BiTree a -> a -> BiTree a
biTreeAdd (EmptyTree) x = BiTree x EmptyTree EmptyTree
biTreeAdd (BiTree v t1 t2) x = if x > v then BiTree v (biTreeAdd t1 x) t2 else BiTree v t1 (biTreeAdd t2 x)

toBiTree ::(Ord a) => [a] -> BiTree a
toBiTree [] = EmptyTree
toBiTree x = foldl (biTreeAdd) EmptyTree x
