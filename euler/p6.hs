sumOfSquares :: Integer -> Integer
squareOfSums :: Integer -> Integer
sumSquareDifference :: Integer -> Integer
sumSquareDifferenceOf100 :: Integer

sumOfSquares n = n*(n+1)*(2*n+1) `div` 6
squareOfSums n = (n*(n+1) `div` 2) ^ 2

sumSquareDifference n = (squareOfSums n) - (sumOfSquares n)

sumSquareDifferenceOf100 = sumSquareDifference 100