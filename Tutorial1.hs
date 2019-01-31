module Tutorial1 where

import Test.QuickCheck

-- 2.2 curried functions --
mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z

myaddPair :: (Int, Int) -> Int
myaddPair (x,y) = x + y

myadd :: Int -> Int -> Int
myadd x y = x + y

myadd1 :: Int -> Int
myadd1 = myadd 1

take5 :: [Int] -> [Int]
take5 = take 5

{- Note: replace 'undefined' with your own implementations -}

curry1 :: ((a, b) -> c) -> a -> b -> c
curry1 f a b = f (a,b) --TODO Q1

uncurry1 :: (a -> b -> c) -> (a, b) -> c
uncurry1 f (a,b) = f a b --TODO Q1

prop_curry_add :: Int -> Int -> Bool
prop_curry_add x y = curry1 (uncurry1 myadd) x y == myadd x y --TODO Q6

-- 2.3 parametric polymorphism --
identity x = x

-- 2.4 define functions --
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

--use the function error to print out error msg
hd :: [a] -> a
hd []        = error "cannot take the head of an empty list!"
hd (x:xs)    = x

first :: (a,b) -> a
first (x,y) = x --TODO Q2

isZero :: Int -> Bool
isZero 0 = True
isZero _ = False --TODO Q2

-- lambda expressions --
add = \x -> (\y -> x + y)
myconst x = \_ -> x
odds n = map (\x -> x*2 + 1) [0..n-1]

-- Exercise --
isleapyear :: Int -> Bool
isleapyear x = if x `mod` 4 == 0 then
  if x `mod` 100 == 0 then
    if x `mod` 400 == 0 then True else False
  else True
  else False

isleap :: Int -> Bool
isleap j
  | 0 == j `mod` 100 = 0 == j `mod` 400
  | otherwise = 0 == j `mod` 4

--use Property type if you want to limit the input argument
--pay attention to the ==> arrow, which is not =>
prop_leap :: Int -> Property
prop_leap j = 
  (j > 0) ==>
  (isleap j) == (isleapyear j) --TODO Q5

-- Exercise --

--can use the function null to determine whether a list is empty
safetail1 :: Eq a => [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

safetail2 ::  Eq a => [a] -> [a]
safetail2 xs
  | null xs = []
  | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs

safetail4 :: Eq a => [a] -> [a]
safetail4 = \xs -> if null xs then [] else tail xs


-- More on Quickcheck --
init2 :: [Int] -> [Int]
init2 xs = take (length xs -1) xs

prop_init :: [Int] -> Bool
prop_init xs = (init xs) == (init2 xs)

prop_init2 :: [Int] -> Property
prop_init2 xs =
    (length xs > 0) ==>
    (init xs) == (init2 xs)

prop_init3 :: [Int] -> Property
prop_init3 xs = let n = length xs
                in
                   n > 0
                    ==> (init xs) == (init2 xs)

prop_init4 :: [Int] -> Property
prop_init4 xs = n > 0
                 ==> (init xs) == (init2 xs)
                where
                 n = length xs
