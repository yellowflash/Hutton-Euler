xpow 0 _ = 1
xpow n x = x * (xpow (n-1) x)

andx = (foldr (&&) True)
concatx = foldr (++) []

replicatex 0 _ = []
replicatex n x = x:(replicatex (n-1) x)

nth n (x:xs) | n == 0 = x
             | x > 0  = nth (n-1) xs

elemx _ [] = False
elemx n (x:xs) | n == x = True
             | otherwise = elemx n xs

merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x:(merge xs (y:ys))
                    | otherwise = y:(merge (x:xs) ys)

mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort (take mid xs)) (mergesort (drop mid xs))
               where mid = (length xs) `div` 2

lastx [x] = x
lastx (x:xs) = lastx xs