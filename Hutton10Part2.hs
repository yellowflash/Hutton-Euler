import Data.List
data Nat = Zero | Suc Nat

instance Show Nat where
         show n = (show (natToInt n))


intToNat 0 = Zero
intToNat n = Suc (intToNat (n-1))

natToInt Zero = 0
natToInt (Suc n) = 1 + (natToInt n)

add Zero n = n
add (Suc n) n' = Suc (add n n')

mult Zero _ = Zero
mult _ Zero = Zero
mult (Suc Zero) n = n
mult (Suc n) n' = (add (mult n n') n')

data Tree = Leaf Int | Node Tree Tree
            deriving Show

noOfLeaves (Leaf _) = 1
noOfLeaves (Node left right) = (noOfLeaves left) + (noOfLeaves right)

balanced (Leaf _) = True
balanced (Node left right) = abs (noOfLeaves left - noOfLeaves right) <= 1

split xs = ((take mid xs),(drop mid xs))
           where mid = (length xs) `div` 2

doBalance xs = case xs of
                  [a] -> (Leaf a)
                  xs -> Node (balance lsplit) (balance rsplit)
               where (lsplit,rsplit) = split xs
balance = doBalance . sort