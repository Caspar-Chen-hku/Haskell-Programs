triangle :: Int -> Int
triangle 0 = 0
triangle n = n + triangle (n-1)

triangle1 :: Int -> Int
triangle1 n = foldr (+) 0 [0..n]

triangle2 :: Int -> Int
triangle2 n = length [ 1 | x<-[1..n], y<-[1..x]]

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n (x:xs)
  | n<=x = n:(x:xs)
  | otherwise = x : insert n xs

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert x (isort xs)

smaller :: Int -> [Int] -> [Int]
smaller n xs = [ x | x<-xs, x<n]

larger :: Int -> [Int] -> [Int]
larger n xs = [ x | x<-xs , x>n]

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort (smaller x xs) ++ [x] ++ qsort (larger x xs)


data Tree = Leaf Int | Node Tree Tree deriving Show

t1 = Leaf 1
t2 = Node (Leaf 2) (Leaf 3)
t3 = Node (Node (Leaf 4) (Leaf 5)) (Leaf 6)

leaves :: Tree -> [Int]
leaves (Leaf x) = [x]
leaves (Node x y) = leaves x ++ leaves y

size :: Tree -> Int
size (Leaf _) = 1
size (Node x y) = size x + size y

balanced :: Tree -> Bool
balanced (Leaf _) = True
balanced (Node x y) = balanced x && balanced y && size x == size y

halve :: [a] -> ([a],[a])
halve xs = (take l xs, drop l xs) where l = length xs `div` 2

balance :: [Int] -> Tree
balance [x] = Leaf x
balance xs = Node (balance (fst r)) (balance (snd r)) where r = halve xs

data Prop = X | F | T | Not Prop | And Prop Prop deriving (Show, Eq)

eval :: Prop -> Bool -> Bool
eval X b = b
eval (Not p) b = not (eval p b)
eval (And p1 p2) b = eval p1 b && eval p2 b
eval F b = False
eval T b = True

simplify :: Prop -> Prop
simplify (Not T) = F
simplify (Not F) = T
simplify (Not (Not p)) = simplify p
simplify (Not X) = Not X
simplify (Not p) = simplify (Not (simplify p))
simplify (And p1 p2)
  | p1==T = simplify p2
  | p1==F || p2==F = F
  | p2==T = simplify p1
  | p1==p2 = simplify p1
  | p1==X && p2==X = And p1 p2
  | p1==Not p2 || p2==Not p1 = F
  | otherwise = simplify (And (simplify p1) (simplify p2))
simplify x = x