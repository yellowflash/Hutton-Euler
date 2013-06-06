add a b = a+b

myDrop n xs = if null xs || n == 0 
       then xs 
       else myDrop (n-1) (tail xs)

myTake n xs = if null xs || n == 0
       then []
       else (head xs):(myTake (n-1) (tail xs))

isOdd = (== 0).(`mod` 2)

myOr a b = if a then True else b

lastButOne xs = if null (tail (tail xs)) then head xs else lastButOne (tail xs)

type BookId = Integer
type BookTitle = String
type BookAuthor = String
type BookAuthors = [BookAuthor]

data BookInfo = Book BookId BookTitle BookAuthors
     deriving (Show)

bookId (Book id _ _) = id


data Customer = Customer{
     customerId :: String,
     customerName :: String,
     customerAddress :: String
} deriving (Show)


data List a = Cons a (List a)
     | Nil
     deriving (Show)

throwError s = if s == "Hello" then error s else "Hello"


safeSecond :: [a] -> Maybe a
safeSecond (_:x:_) = Just x
safeSecond _ = Nothing

lend  :: Double -> Double -> Maybe Double

lend amount balance = if newbalance < 100      
     then Nothing
     else Just newbalance
     where newbalance = balance - amount

lendOther amount balance = let newbalance = balance - amount 
          in 
          if newbalance < 100
          then Nothing
          else Just newbalance

pluralize :: [Int] -> [String]

pluralize = map plural 
          where
                plural 0 = "None"
                plural 1 = "One"
                plural _ = "Many"


defaultValue d x = case x of
                        Just a -> a
                        Nothing -> d

removeDoubles (x:y:xs) | x == y = x:(tail (removeDoubles (y:xs)))
removeDoubles (x:xs) = x:(removeDoubles xs)
removeDoubles [] = []

lendPattern :: Double -> Double -> Maybe Double

lendPattern amount balance | (balance-amount > 100) = Just (balance - amount)
lendPattern _ _ = Nothing
          
