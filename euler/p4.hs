isPalindrome a = a == (reverse a)

isPalindromeNumber a = isPalindrome (show a)

threeDigitPalindromeProducts = filter isPalindromeNumber [x*y | x <- [100..999], y <- [100..999]]

largestThreeDigitPalindromeProduct = maximum threeDigitPalindromeProducts