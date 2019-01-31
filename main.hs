import Control.Exception

f [] = []
f (x:xs) = f ys ++ [x] ++ f zs where
    ys = [ a | a <- xs, a<=x ]
    zs = [ b| b <- xs, b>x ]

n = a `div` length xs where
    a = 10
    xs = [1..5]

fac 0 = 1
fac n = n * fac (n-1)

myprod [] = 1
myprod (n:ns) = n * myprod ns

mylen [] = 0
mylen (x:xs) = 1 + mylen xs

myrev [] = []
myrev (x:xs) = myrev xs ++ [x]

myzip [] ys = []
myzip xs [] = []
myzip (x:xs) (y:ys) = [(x,y)] ++ myzip xs ys

mydrop 0 xs = xs
mydrop n (x:xs) = mydrop (n-1) xs

qsort [] = []
qsort (x:xs) = qsort small ++ [x] ++ qsort large where
  small = [a | a<-xs, a<=x ]
  large = [b | b<-xs, b>x]

myand [True] = True
myand [False] = False
myand (x:xs) = x==True && myand xs

--myconcat :: [[a]] -> [a]
--myconcat [(x:xs)] = (x:xs)
--myconcat [xs:yss] = xs ++ myconcat yss

myrep 0 a = []
myrep n a = [a] ++ myrep (n-1) a

mysel (x:xs) 0 = x
mysel (x:xs) n = mysel xs (n-1)

myelem :: Eq a => a -> [a] -> Bool
myelem a [] = False
myelem a (x:xs) = a==x || myelem a xs

mymerge :: Ord a => [a] -> [a] -> [a]
mymerge [] ys = ys
mymerge xs [] = xs
mymerge (x:xs) (y:ys) = if x<y then [x] ++ mymerge xs (y:ys) 
    else [y] ++ mymerge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort (x:xs) = mymerge [x] (msort xs)

showlast xs = xs !! (length xs -1)

showend xs = head (reverse xs)

init1 xs = take (length xs -1) xs

init2 xs = reverse (tail (reverse xs))

myconcat :: [a] -> [a] -> [a]
myconcat [] ys = ys
myconcat (x:xs) ys = x: (myconcat xs ys)

myconcat1 :: [[a]] -> [a]
myconcat1 [[]] = []
myconcat1 (x:xs) = x ++ myconcat1 xs

main = do
    result <- try (evaluate (5 `div` 2)) :: IO (Either SomeException Int)
    case result of
        Left ex -> putStrLn $ "caught exception: " ++ show ex
        Right val -> putStrLn $ "the answer was: " ++ show val
    print(f(5:[3,6,8]))
    print(n)
    print(showlast [1..5])
    print(showend [1..5])
    print(init1 [1..5])
    print(init2 [1..5])