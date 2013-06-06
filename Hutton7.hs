import Data.Char
import Data.List

foldrx sofar fun [] = sofar
foldrx sofar fun (x:xs) = fun (foldrx sofar fun xs) x

foldlx sofar fun [] = sofar
foldlx sofar fun (x:xs) = foldlx (fun sofar x) fun xs

bitToInt :: [Int] -> Int
bitToInt = foldr (\n sofar -> sofar*2 + n) 0

intToBit :: Int -> [Int]
intToBit 0 = []
intToBit x = (x `mod` 2) :(intToBit (x `div` 2))

make8 :: [Int] -> [Int]
make8 bits = take 8 (bits ++ repeat 0)

encode :: [Char] -> [Int]
encode = concat . map (make8 . intToBit . ord)

chop8 :: [Int] -> [[Int]]
chop8 [] = []
chop8 bits = take 8 bits:chop8 (drop 8 bits)

decode :: [Int] -> [Char]
decode = (map (chr . bitToInt)) . chop8

mapfilter f p = (map f) . (filter p)
allx p = (foldr (&&) True) . (map p)
anyx p = (foldr (||) False) . (map p)
takeWhilex _ [] = []
takeWhilex p (x:xs)  | (p x) = x:(takeWhile p xs)
                    | otherwise = []
dropWhilex _ [] = []
dropWhilex p (x:xs) | (p x) = dropWhilex p xs
                    | otherwise = xs

mapx f = foldr (\x sofar -> (f x):sofar) []
filterx p = foldr (\x sofar -> (if (p x) then (x:sofar) else sofar)) []

dec2int = foldl (\sofar x -> sofar *10 + x) 0

--sumsqreven = compose [sum, map (^2), filter even]
curryx f = \x y -> f (x,y)
uncurryx f = \(x,y) -> (f x y)