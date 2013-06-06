fibonacci = nextFibonacci 0 1
          where nextFibonacci a b = let next = a+ b in
                                    next:(nextFibonacci b next)

sumOfFibonacci = sum (filter even (takeWhile ((>) 4000000) fibonacci))