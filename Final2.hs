data Answer = Yes | No | Unknown deriving Show

flip' :: Answer -> Answer
flip' Yes = No
flip' No = Yes
flip' Unknown = Unknown

data Shape = Circle Float | Rect Float Float deriving Show

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect m n) = m*n

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:_) = Just x

data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = nat2int n + 1

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

data Expr = Val Int
        | Add Expr Expr
        | Mul Expr Expr
        deriving Show

e = Add (Mul (Add (Val 1) (Val 2)) (Val 3)) (Mul (Val 5) (Add (Val 6) (Val 7)))

size :: Expr -> Int
size (Val _) = 1
size (Add e1 e2) = size e1 + size e2
size (Mul e1 e2) = size e1 + size e2

eval :: Expr -> Int
eval (Val n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

fold :: (Int->b) -> (b->b->b) -> (b->b->b) -> Expr -> b
fold f a m (Val n) = f n
fold f a m (Add e1 e2) = a (fold f a m e1) (fold f a m e2)
fold f a m (Mul e1 e2) = m (fold f a m e1) (fold f a m e2)

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show
t  = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x==y
occurs x (Node t1 n t2)
    | x==n = True
    | x<n = occurs x t1
    | otherwise = occurs x t2

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node t1 x t2) = flatten t1 ++ [x] ++ flatten t2

treesize :: Tree a -> Int
treesize (Leaf _) = 1
treesize (Node t1 x t2) = treesize t1 + treesize t2 + 1

complete :: Tree a -> Bool
complete (Leaf _) = True
complete (Node t1 x t2) = treesize t1 == treesize t2 && complete t1 && complete t2
