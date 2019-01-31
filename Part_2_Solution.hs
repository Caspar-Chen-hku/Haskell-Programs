module Tutorial1Solution where

import Test.QuickCheck

-- curried functions --
mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z

myaddPair :: (Int, Int) -> Int
myaddPair (x, y) = x + y

myadd :: Int -> Int -> Int
myadd x y = x + y

myadd1 :: Int -> Int
myadd1 = myadd 1

take5 :: [Int] -> [Int]
take5 = take 5

curry1 :: ((a, b) -> c) -> a -> b -> c
curry1 f = \x -> \y -> f (x,y)

uncurry1 :: (a -> b -> c) -> (a, b) -> c
uncurry1 f = \(x,y) -> f x y

prop_curry_add :: Int -> Int -> Bool
prop_curry_add x y = curry1 (uncurry1 myadd) x y == myadd x y

-- parametric polymorphism --
identity x = x

-- define functions --
-- conditional expressions --
signum1 :: Int -> Int
signum1 n = if n < 0 then -1 else
    if n == 0 then 0 else 1
-- guarded equations --
signum2 n
    | n < 0 = -1
    | n == 0 = 0
    | otherwise = 1
-- pattern matching --
hd :: [a] -> a
hd []        = error "cannot take the head of an empty list!"
hd (x:xs)    = x

first :: (a,b) -> a
first (x,y) = x

isZero :: Int -> Bool
isZero 0 = True
isZero n = False

-- lambda expressions --
add = \x -> (\y -> x + y)
myconst x = \_ -> x
odds n = map (\x -> x*2 + 1) [0..n-1]

-- Exercise --
isleapyear :: Int -> Bool
isleapyear j =
    if j `mod` 4 /= 0 then False
        else if j `mod` 100 /= 0 then True
            else if j `mod` 400 == 0 then True else False

isleap :: Int -> Bool
isleap j
    | 0==j`mod`100 = 0 == j`mod`400
    | otherwise    = 0 == j`mod`4

prop_leap :: Int -> Property
prop_leap j = (j > 0) ==> isleap j == isleapyear j

-- Exercise --
safetail1 :: [a] -> [a]
safetail1 xs = if (null xs) then [] else tail xs

safetail2 xs
    | null xs = []
    | otherwise = tail xs

safetail3 [] = []
safetail3 (x:xs) = xs

safetail4 = \xs -> if null xs then [] else tail xs

-- More on Quickcheck --
-- for Part1, Q4 --
{- Use Integer instead of Int, or define the property function as prop_absolute or prop_absolute'
-}

absolute :: Int -> Int
absolute x = if (x < 0) then -x else x

prop_absolute :: Int -> Int -> Property
prop_absolute a b = let a' = toInteger a
                        b' = toInteger b
                        maxInt = toInteger (maxBound :: Int)
                        minInt = toInteger (minBound :: Int)
                    in
                        (a' * b' < maxInt && a' * b' > minInt)
                        ==> (absolute a) * (absolute b) == absolute (a * b)

prop_absolute' :: Int -> Int -> Property
prop_absolute' a b = (a' * b' < maxInt && a' * b' > minInt)
                        ==> (absolute a) * (absolute b) == absolute (a * b)
                    where
                        a' = toInteger a
                        b' = toInteger b
                        maxInt = toInteger (maxBound :: Int)
                        minInt = toInteger (minBound :: Int)


