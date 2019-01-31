import Prelude hiding ( (!!),(++),(.) )

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs++ys)

(.) :: (b->c) -> (a->b) -> (a->c)
f . g = \x -> f (g x)

mysum :: Num a => [a] -> a
mysum [] = 0
mysum (x:xs) = x + mysum xs

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort former ++ [x] ++ qsort latter where
    former = [ n | n<-xs, n<x]
    latter = [ n | n<-xs, n>=x]

myhead :: [a] -> a
myhead (x:xs) = x

mytail :: [a] -> [a]
mytail (x:xs) = xs

(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(x:xs) !! n = xs !! (n-1)

mytake :: Int -> [a] -> [a]
mytake 0 _ = []
mytake n (x:xs) = x : mytake (n-1) xs

mydrop :: Int -> [a] -> [a]
mydrop 0 xs = xs
mydrop n (x:xs) = mydrop (n-1) xs

mylength :: [a] -> Int
mylength [] = 0
mylength (x:xs) = 1 + mylength xs

myproduct :: [Int] -> Int
myproduct [] = 1
myproduct (x:xs) = x * myproduct xs

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

mylast :: [a] -> a
mylast = head . reverse

myinit :: [a] -> [a]
myinit = reverse . tail . reverse

mynot :: Bool -> Bool
mynot True = False
mynot False = True

myIsDigit :: Char -> Bool
myIsDigit x = ((x >= 'a') && (x <= 'z')) || ((x >= 'A') && (x <= 'Z'))

myfst :: (a,b) -> a
myfst (a,b) = a

myzip :: [a] -> [b] -> [(a,b)]
myzip (x:xs) (y:ys) = (x,y) : (myzip xs ys)
myzip _ _ = []

second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

pair :: a -> b -> (a,b)
pair a b = (a,b)

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a->a) -> a -> a
twice f x = f (f x)

mysignum :: Int -> Int
mysignum a
    | a<0 = -1
    | a==0 = 0
    | otherwise = 1

mysignum' :: Int -> Int
mysignum' a = if a<0 then -1 else
    if a==0 then 0 else 1

myconst :: a -> b -> a
myconst a = \_ -> a

odds :: Int -> [Int]
odds n = map (\x -> x^2 + 1) [0..n-1]

safetail :: [a] -> [a]
safetail [] = []
safetail xs = tail xs

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

myand :: [Bool] -> Bool
myand [] = True
myand (x:xs) = x && myand xs

myor :: [Bool] -> Bool
myor [] = False
myor (x:xs) = x || myor xs

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n x = x : myreplicate (n-1) x

myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem x (n:ns) = x==n || myelem x ns

merge :: Real a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x<y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

msort :: Real a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort former) (msort latter) where
    l = length xs `div` 2
    former = take l xs
    latter = drop l xs

factors :: Int -> [Int]
factors x = [ n | n<-[1..x], x `mod` n == 0]

prime :: Int -> Bool
prime x = length (factors x) == 2

primes :: Int -> [Int]
primes n = [ x | x <- [2..n], prime x]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [ a<b | (a,b) <- pairs xs] 

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [ n | (n,m) <- zip [0..l] xs, x==m] where l = length xs - 1

count :: Eq a => a -> [a] -> Int
count x xs = length [ n | n<-xs, n==x]

pyths :: Int -> [(Int,Int,Int)]
pyths n = [ (x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x^2+y^2==z^2]

perfects :: Int -> [Int]
perfects n = [ x | x<-[2..n], sum (factors x) == 2*x]

scalarProd :: Real a => [a] -> [a] -> a
scalarProd xs ys = sum [ x*y | (x,y) <- zip xs ys]

mymap :: (a->b) -> [a] -> [b]
mymap f xs = [f x | x<-xs] 

myfilter :: (a->Bool) -> [a] -> [a]
myfilter p xs = [ x | x<-xs, p x]

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f v [] = v
myfoldr f v (x:xs) = f x (myfoldr f v xs)

myall :: (a -> Bool) -> [a] -> Bool
myall p xs = and [ p x | x<-xs]

myany :: (a->Bool) -> [a] -> Bool
myany p xs = or [ p x | x<-xs ]

mytakeWhile :: (a->Bool) -> [a] -> [a]
mytakeWhile p [] = []
mytakeWhile p (x:xs)
    | p x = x : mytakeWhile p xs
    | otherwise = []

mydropWhile :: (a->Bool) -> [a] -> [a]
mydropWhile p [] = []
mydropWhile p (x:xs)
    | p x = mydropWhile p xs
    | otherwise = (x:xs)

map' :: (a->b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

foldr' :: (a->Bool) -> [a] -> [a]
foldr' p = foldr (\x y -> if p x then x:y else y) []