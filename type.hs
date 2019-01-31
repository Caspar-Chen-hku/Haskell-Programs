--type declaration
type Pos = (Int, Int)

origin :: Pos
origin = (0,0)

left :: Pos -> Pos
left (x,y) = (x-1,y)

--type declaration can also have parameters
--but they cannot be recursive
type Pair a = (a,a)

mult :: Pair Int -> Int
mult (m,n) = m*n

--type with two values
--False and True -> constructor
--type and constructor name must start with uppercase
data Answer = Yes | No | Unknown
    deriving Show

answers :: [Answer]
answers = [Yes,No,Unknown]

--constructors can also have parameters
data Shape = Circle Float | Rect Float Float
    deriving Show

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

--data declarations can also have parameters
data Maybe' a = Nothing' | Just' a
    deriving Show

safediv :: Int -> Int -> Maybe' Int
safediv _ 0 = Nothing'
safediv m n = Just' (m `div` n)

safehead :: [a] -> Maybe' a
safehead [] = Nothing'
safehead (x:xs) = Just' x

--data type can be recursive
--it has infinite number of values
data Nat = Zero | Succ Nat
    deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = nat2int n + 1

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add m n = int2nat (nat2int m + nat2int n)

data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr
    deriving Show

size :: Expr -> Int
size (Val n) = 1
size (Add a b) = size a + size b
size (Mul a b) = size a + size b

eval :: Expr -> Int
eval (Val n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

--binary tree
data Tree a = Leaf a
            | Node (Tree a) a (Tree a)
    deriving Show

occurs :: Ord a => a -> Tree a -> Bool
occurs a (Leaf b) = if (a==b) then True else False
occurs x (Node a b c) = occurs x a || x==b || occurs x c

flatten :: Tree a -> [a]
flatten (Leaf a) = [a]
flatten (Node a b c) = flatten a ++ [b] ++ flatten c

occursST :: Ord a => a -> Tree a -> Bool
occursST a (Leaf b) = if (a==b) then True else False
occursST x (Node a b c)
    | x<b = occursST x a
    | x==b = True
    | otherwise = occursST x c

constructTree :: [a] -> Tree a
constructTree [a] = Leaf a
constructTree xs = Node (constructTree a) b (constructTree c) where
    x = (length xs -1) `div` 2
    a = take x xs
    b = xs!!x
    c = drop (x+1) xs 

equalTree :: Eq a => Tree a -> Tree a -> Bool
equalTree (Leaf x) (Leaf y) = x==y
equalTree (Node a b c) (Node x y z) = equalTree a x && b==y && equalTree c z

--leftTree :: Tree a -> b
--leftTree (Leaf a) = False
--leftTree (Node a b c) = a


multiply :: Int -> Int -> Int
multiply 0 _ = 0
multiply _ 0 = 0
multiply a b = multiply a (b-1) + a

complete :: Tree a -> Bool
complete (Leaf a) = True
complete (Node a b c) = complete a && complete c && x == y where
    x = length (flatten a)
    y = length (flatten c)

data Parser a = P (String -> [(a,String)])

item :: Parser Char
item = P (\inp -> if inp=="" then [('\n',"")] else [(inp!!0,inp)])
