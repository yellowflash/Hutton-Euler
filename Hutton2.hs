n = a `div` length xs
  where
        a = 10
        xs = [1, 2, 3, 4, 5]

xlast (x:xs) = if null xs then x else xlast xs

xinit (x:xs) = if null xs then [] else x:(xinit xs)

xinit2 (x:[]) = []
xinit2 (x:xs) = x:(xinit2 xs)

add (a,b) = a+b

add2 a = fst a + snd a