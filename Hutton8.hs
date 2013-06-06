import Data.Char

type Parser a = String -> [(a,String)]

returnx :: a -> Parser a
returnx v = \inp -> [(v,inp)]

failure :: Parser a
failure = \x -> []

item  :: Parser Char
item = \x -> case x of 
                  [] -> []
                  (x:xs) -> [(x,xs)]

parse :: Parser a -> String -> [(a,String)]
parse parser x = parser x

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
parsera >>= next = \inp -> case parse parsera inp of
                                [] -> []
                                [(x,xs)] -> parse (next x) xs

p = item Main.>>= (\a -> item Main.>>= (\b -> item Main.>>= (\c -> returnx (a,c))))

(+++) :: Parser a -> Parser a -> Parser a
parsera +++ parserb = \inp -> case (parse parsera inp) of 
                                   [] -> (parse parserb inp)
                                   r -> r

                                   
sat :: (Char -> Bool) -> Parser Char
sat p = \inp -> case (parse item inp) of
                     [] -> []
                     [(v,inp)] | (p v) -> [(v,inp)]
                               | otherwise -> []

digit = sat isDigit
lower = sat isLower
upper = sat isUpper
letter = sat isAlpha
alphanum = sat isAlphaNum
char x = (sat (== x))
string [] = returnx []
string (x:xs) = char x Main.>>= (\a -> string xs Main.>>= (\b -> returnx (a:b)))

many parser = (many1 parser) +++ returnx []
many1 parser = parser Main.>>= (\v -> many parser Main.>>= (\vs -> returnx (v:vs)))

ident = lower Main.>>= (\a -> many alphanum Main.>>= (\as -> returnx (a:as)))
nat :: Parser Int
nat = (many1 digit) Main.>>= (\d -> returnx (read d))
space :: Parser ()
space = many (sat isSpace) Main.>>= (\d -> returnx ())

token :: Parser a -> Parser a
token parser = space Main.>>= (\a -> parser Main.>>= (\b -> space Main.>>= (\c -> returnx b)))

identifier = token ident

natural:: Parser Int
natural = token nat
integer :: Parser Int
integer = (token (char '-') Main.>>= (\a -> natural Main.>>= \b-> returnx (-b))) +++ natural 


evaluate x = parse expression x

sumexp = (integer Main.>>= (\a -> token (char '*') Main.>>= (\b -> sumexp Main.>>= \c -> returnx (a*c)))) +++ integer
expression = (sumexp Main.>>= (\a -> token (char '+') Main.>>= (\b -> expression Main.>>= \c -> returnx (a+c)))) +++ sumexp

main = do
        exp <- getLine
        case (evaluate exp) of
             [] -> putStr "Error evaluating the exp"
             [(val,[])] -> putStr (show val)
             [(_,uncomsumed)] -> putStr "There are unconsumed input"

