smallestMultipleOf (x:xs) = x * (smallestMultipleOf (map (wholeDivide x) xs))
                            where wholeDivide a b | b `mod` a == 0  =  b `div` a
                                  wholeDivide _ b                   =  b
smallestMultipleOf _ = 1                     

smallestMultipleOf1To20 = smallestMultipleOf [1..20]