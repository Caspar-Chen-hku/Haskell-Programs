module Tutorial0 where

import Test.QuickCheck

-- Q1 --
{- Invalid. In Haskell, variable names should start with lowercase letters.
-}


-- Q2 --
last1 :: [Int] -> Int
last1 xs = xs !! (length xs - 1)

last2 :: [Int] -> Int
last2 xs = head (reverse xs)

-- Q3 --
init1 :: [Int] -> [Int]
init1 xs = take (length xs - 1) xs

init2 :: [Int] -> [Int]
init2 xs = reverse (tail (reverse xs))

-- Q4 --
{- It is possible to fail due to Int overflow. You can fix it by using another type instead of Int, or use the technology we present in 2.5, Part 2. I will tell you more in the Part 2 Solution. -}


-- Q5 --
prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs
