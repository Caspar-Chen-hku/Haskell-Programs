
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


item :: Parser Char
item = P (\inp -> if null inp then [] else [(head inp, tail inp)])

failure :: Parser a
failure = P (\inp -> [])

return' :: a -> Parser a
return' v = P (\inp -> [(v,inp)])

(+++) :: Parser a -> Parser a -> Parser a
f +++ g = P (\inp ->
    let r1 = parse f inp in
        if null r1 then parse g inp else r1)


parse :: Parser a -> String -> [(a,String)]
parse (P f) s = f s

p :: Parser (Char,Char)
p = do a <- item
       item
       b <- item
       return (a,b)

sat :: (Char -> Bool) -> Parser Char
sat p = do i <- item
           if p i then return' i else failure


digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char c = sat (==c)

many :: Parser a -> Parser [a]
many p = many1 p +++ return' []

many1 :: Parser a -> Parser [a]
many1 p = do x <- p
             y <- many p
             return (x:y)

firstAlpha :: Parser Char
firstAlpha = sat isAlpha

manyN :: Int -> Parser a -> Parser [a]
manyN 0 p = return' []
manyN i p = do x <- p
               y <- manyN (i-1) p
               return (x:y)

space :: Parser ()
space =  do many (sat isSpace)
            return ()

-- Ignoring spacing

token  :: Parser a -> Parser a
token p =  do space
              v <- p
              space
              return v

nat :: Parser Int
nat =  do xs <- many1 digit
          return (read xs)

int :: Parser Int
int =  do char '-'
          n <- nat
          return (-n)
        +++ nat

integer :: Parser Int
integer =  token int

data Expr = Val Int 
            | Add Expr Expr 
            | Mul Expr Expr
            deriving Show

binary :: String -> String -> String -> String
binary op x y = "(" ++ x ++ " " ++ op ++ " " ++ y ++ ")"

printTree :: Expr -> String
printTree (Val n) = show n
printTree (Add x y) = binary "+" (printTree x) (printTree y)
printTree (Mul x y) = binary "*" (printTree x) (printTree y)

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

pExpr :: Parser Expr
pExpr = do t <- token pTerm
           (do char '+'
               e <- token pExpr
               return (Add t e)) +++ return t

pTerm :: Parser Expr
pTerm = (do f <- token pFactor
            char '*'
            t <- token pTerm
            return (Mul f t)) +++ pFactor

pFactor = (do token (char '(')
              e <- pExpr
              token (char ')')
              return e) +++ ( do i <- integer
                                 return (Val i))

interpret :: String -> Int
interpret s = eval (fst (head (parse pExpr s)))