import Test.QuickCheck
import Final3
import Data.Char
import System.IO

myaddPair :: (Int,Int) -> Int
myaddPair (a,b) = a+b

myadd :: Int -> Int -> Int
myadd a b = a+b

curry' :: ((a,b) -> c) -> a -> b -> c
curry' f a b = f (a,b)

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f (a,b) = f a b

isleapyear :: Int -> Bool
isleapyear x
    | x `mod` 4 /= 0 = False
    | x `mod` 100 /= 0 = True
    | x `mod` 400 /= 0 = False
    | otherwise = True

isleap :: Int -> Bool
isleap j
    | 0 == j `mod` 100 = 0 == j `mod` 400
    | otherwise = 0 == j `mod` 4

prop_isleapyear :: Int -> Property
prop_isleapyear x = x > 0 
                    ==> isleap x == isleapyear x

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

zipSum :: [Int] -> [Int] -> [Int]
zipSum xs ys = [ x+y | (x,y) <- zip xs ys ]

zipSum' :: [Int] -> [Int] -> [Int]
zipSum' = zipWith (+) 

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = map (\(x,y) -> f x y) (zip xs ys)

length' :: [a] -> Int
length' = foldl (\x _ -> x+1) 0

reverse' :: [a] -> [a]
reverse' = foldl (\x y -> y:x) []

map' :: (a->b) -> [a] -> [b]
map' f = foldl (\x y -> x ++ [f y]) []

filter' :: (a->Bool) -> [a] -> [a]
filter' p = foldl (\x y -> if p y then x ++ [y] else x) []

unzip' :: [(a,b)] -> ([a],[b])
unzip' = foldl (\(xs,ys) (a,b) -> (xs++[a],ys++[b])) ([],[])

sumOdds :: [Int] -> Int
sumOdds = sum . (filter odd) . (filter (>0))

f :: [Int] -> Bool
f = (>100) . sum . (map (\x -> 1 + x*2)) . (filter even)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

t1 :: Tree Int
t1 = Node (Node Leaf 1 Leaf) 3 (Node Leaf 2 Leaf)

t2 :: Tree Int
t2 = Node (Node Leaf 1 Leaf) 4 (Node (Node Leaf 2 Leaf) 3 (Node Leaf 5 Leaf))

numLeaf :: Tree a -> Int
numLeaf Leaf = 1
numLeaf (Node t1 a t2) = numLeaf t1 + numLeaf t2

balanced :: Tree a -> Bool
balanced Leaf = True
balanced (Node t1 a t2) = abs (numLeaf t1 - numLeaf t2) <= 1 && balanced t1 && balanced t2

mapTree :: (a->b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node t1 a t2) = Node (mapTree f t1) (f a) (mapTree f t2)

foldTree :: (a->b->b) -> b -> Tree a -> b
foldTree f v Leaf = v
foldTree f v (Node t1 a t2) = foldTree f (f a (foldTree f v t2)) t1

data Expr 
    = Val Int 
    | Add Expr Expr 
    | Mul Expr Expr
    deriving Show

e = Add (Mul (Val 1) (Val 2)) (Mul (Val 3) (Val 4))

eval :: Expr -> Int
eval (Val x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

fold :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
fold f a m (Val x) = f x
fold f a m (Add e1 e2) = a (fold f a m e1) (fold f a m e2)
fold f a m (Mul e1 e2) = m (fold f a m e1) (fold f a m e2)

binary :: String -> String -> String -> String
binary op s1 s2 = "(" ++ s1 ++ op ++ s2 ++ ")"

printExpr :: Expr -> String
printExpr = fold show (binary "+") (binary "*")

collect' :: Expr -> [Int]
collect' = fold (\x -> [x]) (++) (++)

evalPrint :: Expr -> (Int,String)
evalPrint = fold (\x -> (x,show x)) (\(e1,s1) (e2,s2) -> ((e1+e2),binary "+" s1 s2)) (\(e1,s1) (e2,s2) -> ((e1*e2),binary "*" s1 s2))

firstAlpha :: Parser Char
firstAlpha = sat isAlpha

manyN :: Int -> Parser a -> Parser [a]
manyN 0 p = return []
manyN n p = do x <- p
               xs <- manyN (n-1) p
               return (x:xs)

ispalin :: String -> Bool
ispalin "" = True
ispalin [x] = True
ispalin (x:xs) = x==last xs && ispalin (init xs)

convertStr :: String -> String
convertStr xs = [ toLower x | x<-xs, isAlpha x]

palindrome :: IO ()
palindrome = do word <- getLine
                let r = ispalin (convertStr word) in
                    if r then
                        putStrLn "it's palindrome!"
                    else
                        putStrLn "it's not palindrome!"
