import Control.Exception

myconcat :: [[a]] -> [a]
myconcat xss = [x | xs<-xss, x<-xs ]

factors :: Int -> [Int]
factors n = [x | x<-[1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x<-[2..n], prime x]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [n | n<-[0..(length xs -1)], xs!!n==x]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x'==x]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x^2+y^2==z^2]

perfects :: Int -> [Int]
perfects n = [x | x<-[1..n], sum (factors x) == 2*x]

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x*y | (x,y)<-(zip xs ys)]

mymap :: (a->b) -> [a] -> [b]
mymap f xs = [f x | x <- xs]

myfilter :: (a->Bool) -> [a] -> [a]
myfilter p xs = [x | x<-xs, p x]

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f v [] = v
myfoldr f v (x:xs) = f x (myfoldr f v xs)

mysum :: [Int] -> Int
mysum = myfoldr (+) 0

myproduct :: [Int] -> Int
myproduct = myfoldr (*) 1

mylength :: [a] -> Int
mylength = myfoldr (\_ n -> n+1) 0

mylength1 :: [a] -> Int
mylength1 [] = 0
mylength1 (x:xs) = 1 + mylength1 xs

myreverse :: [a] -> [a]
myreverse = myfoldr (\x1 x2 -> x2 ++ [x1]) []

myreverse1 :: [a] -> [a]
myreverse1 [] = []
myreverse1 (x:xs) = myreverse1 xs ++ [x]

myall :: (a -> Bool) -> [a] -> Bool
myall p xs = and [p x | x<-xs]

myany :: (a -> Bool) -> [a] -> Bool
myany p xs = or [p x | x<-xs]

mytakeWhile :: (a -> Bool) -> [a] -> [a]
mytakeWhile p [] = []
mytakeWhile p (x:xs) = if (p x) then x : mytakeWhile p xs else []

mydropWhile :: (a -> Bool) -> [a] -> [a]
mydropWhile p [] = []
mydropWhile p (x:xs) = if (p x) then mydropWhile p xs else (x:xs)