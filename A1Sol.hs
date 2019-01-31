-- Problem 1

fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- Problem 2

lookupName :: [String] -> String -> [String]
lookupName book prefix =
  if null prefix
    then newBook
    else filter (startWith prefix) newBook
  where
    startWith p s = length p <= length s && and (zipWith (==) p s)
    format  = map (\c -> if c=='-' then ' '; else c)
    newBook = map format book

-- Problem 3

quick_sort :: [Int] -> [Int]
quick_sort [] = []
quick_sort (x:xs) = quick_sort smaller ++ [x] ++ quick_sort larger
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

wave :: [Int] -> [Int]
wave ls = take_half $ interleave (reverse (quick_sort ls)) (quick_sort ls)
  where take_half ls = take (length ls `div` 2) ls
        interleave [] [] = []
        interleave (x:xs) (y:ys) = x : y : interleave xs ys

--Problem 4

isStepped :: [Int] -> Bool
--isStepped [] = False
isStepped xs = (sorted xs) && (and [(length (filter (==x) xs)) >= 3 | x <- xs])

-- Problem 5

isMartian :: [Int] -> Bool
isMartian nums = (countNum nums 1 > countNum nums 2) && (adjacentEqual nums (-1))

countNum :: [Int] -> Int -> Int
countNum [] _ = 0
countNum (x : xs) n = if n == x
	                  then 1 + countNum xs n
	                  else countNum xs n

adjacentEqual :: [Int] -> Int -> Bool
adjacentEqual [] num = True
adjacentEqual (x : xs) num = if num == x then False
                             else adjacentEqual xs x

-- Problem 6

isTwinPaired :: [Int] -> Bool
isTwinPaired nums = (sorted (filter even nums)) && (sorted (filter odd nums))

sorted :: [Int] -> Bool
sorted xs = and $ zipWith (<=) xs (tail xs)

-- Problem 7

intToColumn :: Int -> String
intToColumn 0 = []
intToColumn n =
  intToColumn (m `div` 26) ++ [alphabet !! (m `mod` 26)]
    where
      m = n - 1
      alphabet = reverse ['A'..'Z']

-- Problem 8

-- the pattern of calculator can be regarded as:
--    1) start with an integer
--    2) followed by the pattern: one op and one integer
-- therefore, calculator use 0 as first element.
-- add '+' to the string
-- and let calculator' consume one pattern at one time

index :: Char -> String -> Int
index = go 0
  where go i x [] = error "out of bound" -- impossible case
        go i x (y:ys) | x == y = i
                      | otherwise = go (i + 1) x ys

getInt' :: Int -> String ->(Int, String)
getInt' acc [] = (acc, [])
getInt' acc (x : xs)
     | elem x nums = getInt' (acc * 10 + index x nums) xs
     | otherwise   = (acc, x : xs)
   where nums = ['0'..'9']

getInt :: String ->(Int, String)
getInt = getInt' 0

getOperator :: Char -> Int -> Int -> Int
getOperator '+' = (+)
getOperator '-' = (-)
getOperator '*' = (*)
getOperator '/' = div

calculator' :: Int -> String -> Int
calculator' acc [] = acc
calculator' acc (op:s1) =
  let (num, s2) = getInt s1 in
    if op == '/' && num == 0
    then -1
    else calculator' (getOperator op acc num) s2

calculator :: String -> Int
calculator s =
  case filter (/= ' ') s of
    "" -> 0
    s1  -> calculator' 0 ('+':s1)