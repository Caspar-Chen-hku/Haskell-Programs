import Prelude hiding (concat, and, (!!), replicate, elem)

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n a = [a] ++ replicate (n-1) a

(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(x:xs) !! n = xs !! (n-1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem a (x:xs) = a==x || elem a xs

fib :: Int -> Int
fib 0  = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

doubleList1 :: [Int] -> [Int]
doubleList1 [] = []
doubleList1 (x:xs) =  [2*x] ++ doubleList1 xs 

doubleList2 :: [Int] -> [Int]
doubleList2 xs = map (*2)  xs

zipSum1 :: [Int] -> [Int] -> [Int]
zipSum1 (x:xs) (y:ys) = [x+y] ++ zipSum1 xs ys
zipSum1 _ _ = []

--zipWith: makes a list, its elements are 
--calculated from the function and the elements 
--of input lists occuring at the same position in both lists

--use ^ generate integer while use ** generate Float num
zipWith1 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith1 f (x:xs) (y:ys) = [f x y] ++ zipWith' f xs ys
zipWith1 f _ _ = []

zipSum2 :: [Int] -> [Int] -> [Int]
zipSum2 xs ys =  zipWith (+) xs ys

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x<y = [x] ++ merge xs (y:ys)
  | otherwise = [y] ++ merge (x:xs) ys


--splitAt 2 [1..6] == ([1,2],[3,4,5,6])
msort :: [Int] -> [Int]
msort [] = []
msort [n] = [n]
msort xs = merge (msort (fst splited)) (msort (snd splited)) where
    splited = splitAt (length xs `div` 2) xs

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x^2+y^2==z^2]

factors :: Int -> [Int]
factors n = [x | x<-[1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x<-[2..n], 2*x==sum (factors x)]

filtmap :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtmap f p xs = map (f) (filter p xs)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = map (\(a,b) -> f a b) (zip xs ys)

scalarProd :: Num a => [a] -> [a] -> a
scalarProd xs ys = sum (map (\(a,b) -> a * b) (zip xs ys))

scalarProd' :: Num a => [a] -> [a] -> a
scalarProd' (x:xs) (y:ys) = x*y + scalarProd' xs ys
scalarProd' _ _ = 0

mylength :: [a] -> Int
mylength xs = foldr (\_ n -> n+1) 0 xs

myreverse :: [a] -> [a]
myreverse xs = foldr (\x1 x2 -> x2++[x1]) [] xs

mymap :: (a -> b) -> [a] -> [b]
mymap f xs = foldr (\x1 x2 -> [f x1]++x2) [] xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter p xs = foldr (\x1 x2 -> if (p x1) then [x1]++x2 else x2) [] xs

myunzip :: [(a,b)] -> ([a],[b])
myunzip xs = foldr (\x1 x2 -> ([fst x1] ++ fst x2, [snd x1] ++ snd x2)) ([],[]) xs

myunzip' :: [(a,b)] -> ([a],[b])
myunzip' [] = ([],[])
myunzip' (x:xs) = ([fst x]++ fst y, [snd x] ++ snd y) where y = myunzip' xs