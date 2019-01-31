import Data.List

data Op = Add | Sub | Mul | Div deriving Show

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add x y =  x <= y
valid Sub x y =  x > y
valid Mul x y =  x /= 1 && y /= 1 && x <= y
valid Div x y =  y /= 1 && x `mod` y == 0

data Expr = Val Int | App Op Expr Expr deriving Show

eval :: Expr -> [Int]
eval (Val n)        = [n]
eval (App op e1 e2) = [apply op n1 n2 | n1 <- eval e1, n2 <- eval e2, valid op n1 n2]

append :: a -> [[a]] -> [[a]]
append x [] = []
append x (y:ys) = (x:y) : append x ys

--returns all the subsets of input list
subs                          :: [a] -> [[a]]
subs []                       =  [[]]
subs (x:xs)                   =  yss ++ map (x:) yss
                                 where yss = subs xs

--interleave the element x in every possible position in ys
interleave                    :: a -> [a] -> [[a]]
interleave x []               =  [[x]]
interleave x (y:ys)           =  (x:y:ys) : map (y:) (interleave x ys)
 
perms                         :: [a] -> [[a]]
perms []                      =  [[]]
perms (x:xs)                  =  concat (map (interleave x) (perms xs))

choices                       :: [a] -> [[a]]
choices xs                    =  concat (map perms (subs xs))

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a],[a])]
split xs = [ (take n xs,drop n xs) | n <- [1..l] ] where l = length xs -1

ops :: [Op]
ops = [Add,Sub,Mul,Div]

combine :: Expr -> Expr -> [Expr]
combine l r = [ App op l r | op <- ops ] 

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
            l <- exprs ls,
            r <- exprs rs,
            e <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
    [e | c <- choices ns,
        e <- exprs c,
        eval e == [n]]

type Result = (Expr,Int)

results :: [Int] -> [Result]
results [] =  []
results [n] =  [(Val n,n) | n > 0]
results ns =
    [e | (ls,rs) <- split ns
       , lx      <- results ls -- lx :: Result
       , rx      <- results rs -- rx :: Result
       , e       <- combine' lx rx
    ]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [ (App op l r, apply op x y) | op <- ops, valid op x y] 

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
    [e | ns' <- choices ns,
    (e,m) <- results ns',
    m==n]

