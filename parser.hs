import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

data Tree a = Leaf a
            | Node (Tree a) a (Tree a)
    deriving Show

data Parser a = P (String -> [(a,String)])

instance Functor Parser where
   fmap f (P g) = P (\s -> fmap (\(x,s') -> (f x, s')) (g s))

instance Applicative Parser where
   pure   = return
   (<*>)  = ap

instance Alternative Parser where
   empty = failure
   (<|>) = (+++)

instance Monad Parser where
   return v =  P (\inp -> [(v,inp)])
   p >>= f  =  P (\inp -> case parse p inp of
                             []        -> []
                             [(v,out)] -> parse (f v) out)

instance MonadPlus Parser where
   mzero        =  P (\inp -> [])
   p `mplus` q  =  P (\inp -> case parse p inp of
                                 []        -> parse q inp
                                 [(v,out)] -> [(v,out)])

--item :: Parser Char
--item = P (\inp -> if null inp then [] else [(head inp , tail inp)])

--parse the first character or failure
item :: Parser Char
item = P (\inp -> case inp of 
    "" -> []
    (x:xs) -> [(x,xs)])

--use parse function to test the parsers
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

--always fail, return the empty list
failure :: Parser a
failure = P (\inp -> [])

--always return a constant value without
--consuming the input
return' :: a -> Parser a
return' v = P(\inp -> [(v,inp)])

--if p dont fail, return p's result, otherwise q
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P(\inp -> 
    let x = parse p inp in
    case x of
        [] -> parse q inp
        _ -> x)

--take the first and third character in a string
alternative :: Parser (Char,Char) 
alternative = do x <- item
                 item        
                 y <- item         
                 return (x,y)

--sat :: (Char -> Bool) -> Parser Char
--sat p = P (\inp -> 
--    let x = head inp in
--    if p x then [(x,tail inp)] else [])

--similar to item, but only success if p is satisfied
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return' x else failure

--sat with predicate as isDigit
digit :: Parser Char
digit = sat isDigit

--sat with predicate as equal to 
--the provided character
char :: Char -> Parser Char
char x = sat (==x)

--apply a parser multiple times
many :: Parser a -> Parser [a]
many p = many1 p +++ return' []

--apply it recursively
many1 :: Parser a -> Parser [a]
many1 p = do x <- p
             xs <- many p
             return (x:xs)

--parse the initial word of string 
--if it equals the provided string
string :: String -> Parser String
string [] = return ""
string (x:xs) = 
    do char x
       string xs
       return (x:xs)

--parse as a list, fail otherwise
dList :: Parser String
dList  = do char '['
            d  <- digit
            ds <- many (do char ','
                           digit)
            char ']'
            return (d:ds)

expr :: Parser Int
expr = (do t <- term
           plus <- item
           e <- expr
           return t+e) +++ term

term :: Parser Int
term = (do f <- factor
           mul <- item
           t <- term
           return f*t) +++ factor

factor :: Parser Int
factor = digit +++ (do
  p1 <- item
  e <- expr
  p2 <- item
  return e)

digit' :: Parser Int
digit' = do x <- item
            if isDigit x then return' (x-'0') else failure
