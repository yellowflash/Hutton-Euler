leastTwo :: Int -> Int -> Int -> Int
leastTwo x y z = sum [x,y,z] - maximum [x,y,z]

descending :: [Int] -> Bool
descending [] = True
descending (_:[]) = True
descending (x:y:xs) = x >= y && descending (y:xs)

repli :: [Int] -> Int -> [Int]
repli [] _ = []
repli (x:xs) c = (replicate c x) ++ (repli xs c)

index :: [Char] -> Char -> Int
index (x:xs) c
  | x == c = 0
  | otherwise = (index xs c) + 1

toIntFromChar :: Char -> Int
toIntFromChar c
  | c `elem` numbers = index numbers c
  | otherwise = 0
  where numbers = ['0','1','2','3','4','5','6','7','8','9']

charsToNum :: String -> Int
charsToNum = foldl (\s x -> s*10 + toIntFromChar(x)) 0

pack :: [Int] -> [[Int]]
pack [] = []
pack (x:xs) = (x:takeWhile (== x) xs):(pack (dropWhile (== x) xs))
