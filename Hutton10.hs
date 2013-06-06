import Data.List
import Data.Char

data Prop =  Const Bool
             |Var Char
             |Not Prop
             |And Prop Prop
             |Implies Prop Prop
             deriving Show

vars (Var v) = [v]
vars (Not p) = vars p
vars (And p p') = (vars p) ++ (vars p')
vars (Implies p p') = (vars p) ++ (vars p')
vars (Const _) = []

tautology _ (Const p) = p 
tautology s (Var v) = case lookup v s of Just p -> p

tautology s (Not p) = not (tautology s p)
tautology s (And p p') = (tautology s p) && (tautology s p')
tautology s (Implies p p') = (not (tautology s p)) || ((tautology s p) &&  (tautology s p'))




bools 1 = [[True],[False]]
bools n = [b:bs | b <- [True,False], bs <- bools (n - 1)]

uniquevars = nub . vars

isTaut p = all id [(tautology (zip varsInP b) p) | b <- (bools (length varsInP))]
           where varsInP = uniquevars p

data Parser a = Parser (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (Parser p) s = p s

instance Monad Parser where
         p >>= n = Parser (\inp -> case (parse p inp) of
                                        [] -> []
                                        [(x,xs)] -> parse (n x) xs)
         return a = Parser (\inp -> [(a,inp)])

(+++) :: Parser a -> Parser a -> Parser a
p +++ p' = Parser (\inp -> case (parse p inp) of
                           [] -> (parse p' inp)
                           r -> r)

failure = Parser (\inp -> [])

item :: Parser Char
item = Parser (\inp -> case inp of
                       [] -> []
                       (x:xs) -> [(x,xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = Parser (\inp -> case (parse item inp) of
                             [] -> []
                             [(v,inp)] | (p v) -> [(v,inp)]
                                       | otherwise -> [])
upper = sat isUpper
char x = sat ((==) x)

space :: Parser String
space = many (sat isSpace)

string [] = return []
string (x:xs) = do
                c <- char x
                s <- string xs
                return (c:s)
many :: Parser a -> Parser [a]
many p = (many1 p) +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do
          v <- p
          vs <- (many p)
          return (v:vs)

ident = upper
val = (string "true") +++ (string "false")
token p = do
          space
          v <- p
          space
          return v

identifier :: Parser Prop
identifier = do
             i <- token ident
             return (Var i)

value :: Parser Prop
value = do
        v <- (token val)
        return (Const (read v))

res:: Parser Prop
res = value +++ identifier

ele :: Parser Prop
ele = (do
      (token (char '('))
      e <- conj
      (token (char ')'))
      return e) +++ res

predi :: Parser Prop
predi = (do
        (token (char '~'))
        v <- ele
        return (Not v)) +++ ele

imp :: Parser Prop
imp = (do
       p <- predi
       (token (string "=>"))
       p' <- imp
       return (Implies p p'))+++predi

conj :: Parser Prop
conj = (do
       i <- imp
       (token (char '^'))
       i' <- conj
       return (And i i'))+++imp

main = do
       e <- getLine
       case (parse conj e) of
            [] -> error "Parse error"
            [(parsed,[])] -> putStrLn (show (isTaut parsed))
            [(_,xs)] -> error ("Could not parse the part : "++xs)

