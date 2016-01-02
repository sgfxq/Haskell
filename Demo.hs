qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (h:hx) = [i | i <- hx1, i < h] ++ [h] ++ [i | i <- hx1, i > h]
	where hx1 = qsort hx

main = do
	print (qsort "Qiou Yang")
