prime :: [Integer]
prime = nextPrime [] 2
      where nextPrime soFar next = if not (next `hasFactorsIn` soFar)
                                   then next:(nextPrime (next:soFar) (next + 1))
                                   else nextPrime soFar (next + 1)


hasFactorsIn a xs = any (isFactorOf a) xs
                    where isFactorOf a x = (a `mod` x == 0)

prime10001 = head (drop 1000 prime)