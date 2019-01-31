module Final3 where

import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

--The r and l refer to the associativity, the number you specify 
--refers to the operator precedence. When you don't specify 
--the associativity you get an operator that can be associated 
--only by explicit parenthesis or when the associativity is non-ambiguous.
infixl 5 +++

-- Basic parsers
data Parser a =  P (String -> [(a,String)])

-- The monad of parsers (for the do-notation)

instance Functor Parser where
   fmap f (P g) = P (\s -> fmap (\(x,s') -> (f x, s')) (g s))

instance Applicative Parser where
   pure   = return
   (<*>)  = ap

instance Alternative Parser where
   empty = failure
   (<|>) = (+++)

-- parse (return 3) "Hello" --> [(3,"Hello")]

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

item :: Parser Char
item = P (\inp -> if null inp then [] else [(head inp,tail inp)])

parse :: Parser a -> String -> [(a,String)]
parse (P f) s = f s

failure :: Parser a
failure = P(\inp -> [])

return' :: a -> Parser a
return' v = P(\inp -> [(v,inp)])

(+++) :: Parser a -> Parser a -> Parser a
f +++ g = P(\inp -> let r = parse f inp in
    case r of
        [] -> parse g inp
        _ -> r)  

alternate :: Parser (Char,Char)
alternate = do x <- item
               item
               y <- item
               return (x,y)

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then
            return x
           else
            failure

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char c = sat (==c)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do x <- p
             xs <- many p
             return (x:xs)

string :: String -> Parser String
string [] = return []
string (x:xs) = do n <- char x
                   ns <- string xs
                   return (n:ns)

varName :: Parser String
varName = do x <- sat isAlpha
             xs <- many ((sat isAlpha)+++(sat isDigit))
             return (x:xs)

token :: Parser a -> Parser a
token p = do many (char ' ')
             x <- p
             many (char ' ')
             return x

integer :: Parser Int
integer = do x <- many digit
             return (read x :: Int)

pListOp :: Parser [Int]
pListOp = (do x <- token integer
              xs <- many (do token (char ',')
                             integer)
              return (x:xs)) +++ return []


pList :: Parser [Int]
pList = do token (char '[')
           xs <- pListOp
           token (char ']')
           return xs

pExpr :: Parser Int
pExpr = (do t <- pTerm
            token (char '+')
            e <- pExpr
            return (t+e)) +++ 
        (do t <- pTerm
            token (char '-')
            e <- pExpr
            return (t-e)) +++
        pTerm

pTerm :: Parser Int
pTerm = (do f <- pFactor
            token (char '*')
            t <- pTerm
            return (f*t)) +++ 
        (do f <- pFactor
            token (char '/')
            t <- pTerm
            return (f `div` t)) +++
        pFactor

pFactor :: Parser Int
pFactor = (do token (char '(')
              e <- pExpr
              token (char ')')
              return e) +++ token integer

interpret :: String -> Int
interpret s = (fst.head) (parse pExpr s)

