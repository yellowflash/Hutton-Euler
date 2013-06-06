primeFactorsOf a = [x | x <- (takeWhile ((>) (ceiling (sqrt (fromIntegral a)))) prime), a `mod` x == 0]


prime = nextPrime [] 2
      where nextPrime soFar next = if not (next `hasFactorsIn` soFar)
                                   then next:(nextPrime (next:soFar) (next + 1))
                                   else nextPrime soFar (next + 1)

hasFactorsIn a xs = any (isFactorOf a) xs
                    where isFactorOf a x = (a `mod` x == 0)


factorsOf a = filter (\x -> (mod a x) == 0) [2..(div a 2)]

isPrime a = null (factorsOf a)

primeFactors a = filter isPrime (takeWhile ((>) (floor (sqrt (fromIntegral a)))) (factorsOf a))

largestPrimeFactor a = maximum (primeFactors a)

largestPrimeFactorOf600851475143 = largestPrimeFactor 600851475143