xsum [] = 0
xsum (x:xs) = x + xsum xs

qsort [] = []
qsort (x:xs) = qsort lxs ++ [x] ++ qsort rxs
      where lxs = [a | a <- xs, a <= x]
            rxs = [a | a <- xs, a > x]

xproduct [] = 1
xproduct (x:xs) = x * product xs

rqsort [] = []
rqsort (x:xs) = rqsort lxs ++ [x] ++ rqsort rxs
      where lxs = [a | a <- xs, a >= x]
            rxs = [a | a <- xs, a < x]

rqsort2 xs = reverse (qsort xs)
