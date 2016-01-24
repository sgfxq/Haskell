data Maybe1 a = Just1 a | Nothing1 deriving(Show, Read, Eq)

instance Functor Maybe1 where
	fmap f (Just1 x) = Just1 (f x)
	fmap f Nothing1 = Nothing1

div2 :: Int -> Int
div2 x = div x 2
