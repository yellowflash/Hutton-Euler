import Data.Char

getLines:: Int -> IO [String]

getLines 0 = return []
getLines n = do
             l <- getLine
             ls <- getLines (n-1)
             return (l:ls)

printLines [] = return ()
printLines (x:xs) = do
                    putStrLn x
                    printLines xs

data Parser a = Parser (String -> [(a,String)])

instance Monad Parser where
         return a = Parser(\inp -> [(a,inp)])
         p >>= f = Parser (\inp -> case (parse p inp) of
                                   [] -> []  
                                   [(r,inp')] -> (parse (f r) inp'))
parse (Parser f) inp = (f inp)

p +++ p' = (Parser (\inp -> case (parse p inp) of
                                 [] -> (parse p' inp)
                                 r -> r))

item = (Parser it)
       where it [] = []
             it (x:xs) = [(x,xs)]

failure = Parser (\inp -> [])
sat p = do
         i <- item
         if(p i) then return i else failure

char x = sat (== x)

sume = (do
       (p,e) <- prode
       op <- (char '+' +++ char '-')
       (p',e') <- sume

       return  (1, (if (p == 0 || 1 <= p) then e else ['(']++ e++ [')'])
                  ++ [op] ++
                  (if (op == '-' || 1 < p') then e' else ['(']++ e'++ [')']))) +++ prode
       
prode = (do
       (p,e) <- expe
       op <- (char '*' +++ char '/')
       (p',e') <- prode
       return (2, (if (p == 0 || 2 <= p) then e else ['(']++ e++ [')'])
                  ++ [op] ++
                  (if (p' == 0 || 2 < p') then e' else ['(']++ e'++ [')']))) +++ expe
         
expe = (do
      char '('
      e <- sume
      char ')'
      return e) +++ (do 
                    c <- sat isLower
                    return (0,[c]))

term = sat isLower

simplify e =case (parse sume e) of
                 [((_,e),_)] -> e
                 [] -> error ("Some thing is wrong with" ++ e)

main = do
       num <- getLine
       expr <- getLines (read num)
       printLines (map simplify expr)