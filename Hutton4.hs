import Data.Char
import Data.List
halve xs | (length xs `mod` 2) == 0 = (take mid xs , drop mid xs)
                                      where mid = (length xs) `div` 2

safetail xs = if null xs then [] else tail xs

safetail2 [] = []
safetail2 (x:xs) = xs


orx True _ = True
orx _ True = True
orx _ _ = False

xzip [] _ = []
xzip _ [] = []
xzip (x:xs) (y:ys) = (x,y):(xzip xs ys)

pairs xs = xzip xs (tail xs)

letToInt c = ord c - ord 'a'
intToLet x = chr (x + ord 'a')

shift n c | isLower c = intToLet ((letToInt c+n) `mod` 26)
          | otherwise = c

cipher n cs = [shift n c | c <- cs]

freqtable = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent q d = fromIntegral q / fromIntegral d * 100

count c cs = sum [1 | c' <- cs, c' == c]

freq :: [Char] -> [Double]
freq cs = [percent (count x cs)  (length cs) | x <- ['a'..'z']]

chisqr xs ys = sum [(x-y)^2/y | (x,y) <- zip xs ys]


rotate n xs = drop n xs ++ take n xs

maxshift cs = cipher (-code) cs
              where fq = freq cs
                    stats = [chisqr (rotate s fq) freqtable | s <- [0..25]]
                    minf = minimum stats
                    code = head (elemIndices minf stats)
{-
crack :: [Char] -> [Char]
crack cs =cipher (0-(maxshift cs)) cs
-}

sumSq n = sum (take n [x^2 | x <- [1..]])
xreplicate n a = [a | n' <- [1..n]]

pythogorean n = [(x,y,z) | x <- [1..n], y <- [1..n], z<- [1..n],(x^2 + y^2 == z^2)]

factors n = [x | x <- [1..n], n `mod` x == 0]
perfect n = [x | x <- [1..n], (sum (init (factors x))) == x]

scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]