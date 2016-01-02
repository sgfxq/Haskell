mygroup::Eq a => [a] -> [[a]]
mygroup [] = []
mygroup (h:[]) = [[h]]
mygroup (h:hs) = if h == hs!!0
		then (h: hs1!!0) : (tail hs1)
		else [h] : hs1
			where hs1 = mygroup hs
