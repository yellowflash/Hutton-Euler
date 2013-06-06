import Control.Arrow



type ListAna u x = u -> Either () (x,u)

listana :: ListAna u x -> u -> [x]
listana a u = case a u of 
                   Left () -> []
                   Right (x,u') -> x:(listana a u')

count 0 = Left ()
count n = Right (n,n-1)