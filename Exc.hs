import Data.List
import Data.Char

numOfElements :: [a] -> Integer
numOfElements (x:xs) = 1 + (numOfElements xs)
numOfElements [] = 0


mean :: Integral a => [a] -> Double
mean xs = (fromIntegral (sum xs))/(fromIntegral (length xs))

palindrome :: [a] -> [a] 
palindrome (x:xs) = [x] ++ (palindrome xs) ++ [x]
palindrome [] = []

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

sortLists :: [[a]] -> [[a]]
sortLists = sortBy cmpLength 
          where cmpLength xs ys = compare (length xs) (length ys)

interperse :: a -> [[a]] -> [a]
interperse s [] = []
interperse s (x:[]) = x
interperse s (x:xs) = x ++ (s:(interperse s xs))

data BinaryTree = BinaryTree BinaryTree BinaryTree
     | Nil
heightOfTree :: BinaryTree -> Integer
heightOfTree (BinaryTree left right) = (max (heightOfTree left) (heightOfTree right)) + 1
heightOfTree Nil = 0


data Direction = LeftSide | RightSide | Straight
data Point = Point Double Double
direction :: Point -> Point -> Point -> Direction
directionBetween :: [Point] -> [Direction]
direction (Point x1 y1) (Point x2 y2) (Point x3 y3) = if cosine > 0 
          then RightSide 
          else if cosine < 0 
          then LeftSide 
          else Straight
          where cosine = (x2-x1)*(x3-x2)+(y2-y1)*(y3-y2) 

directionBetween (a:b:c:xs) = (direction a b c):(directionBetween (b:c:xs))
directionBetween _ = []


toInt :: String -> Int
toInt xs = findRest 0 xs
      where findRest a (y:ys) = findRest (a * 10 + (digitToInt y)) ys
            findRest a [] = a