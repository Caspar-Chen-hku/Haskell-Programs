import Parsing
import System.IO
import Data.Char
import Control.Monad

isPalin :: String -> Bool
isPalin "" = True
isPalin [x] = True
isPalin (x:xs) = x == last xs && isPalin (init xs)

normalize :: String -> String
normalize "" = ""
normalize (x:xs)
  | x>='a' && x<= 'z' = x : normalize xs
  | x>='A' && x<='Z' = toLower x : normalize xs
  | otherwise = normalize xs

palindrome :: IO()
palindrome = do s <- getLine
                if isPalin (normalize s) then
                    putStrLn "It's a palindrome!"
                else
                    putStrLn "Nope!"

nim :: IO ()
nim = 
    do putStrLn "The game of Nim begins!"
       play 0 [5,4,3,2,1]

getInt :: IO Int
getInt = do x <- getLine
            return (read x :: Int)

printBoard :: [Int] -> Int -> IO()
printBoard [] _ = putStr ""
printBoard (x:xs) n = do putStr (show n ++ ": ")
                         printLn x
                         printBoard xs (n+1)

printLn :: Int -> IO()
printLn 0 = putStrLn ""
printLn x = do putStr "*" 
               printLn (x-1)

newBoard :: [Int] -> Int -> Int -> [Int]
newBoard xs n row = take row xs ++ [xs!!row - n] ++ drop (row+1) xs

play :: Int -> [Int] -> IO ()
play n xs = do printBoard xs 1
               putStrLn ("Player " ++ show (n+1))
               putStr ("Enter a row number: ")
               row <- getInt
               putStr ("Stars to remove: ")
               x <- getInt
               if x<=0 || x> (xs!!(row-1)) then
                do putStrLn "please enter a valid integer!"
                   play n xs
               else
                let newxs = newBoard xs x (row-1) in
                  if newxs == [0,0,0,0,0] then
                    putStrLn ("player " ++ show (n+1) ++ " wins!")
                  else
                    play ( (n+1) `mod` 2 ) newxs

type Uid = String
data Person = Student {name :: String, age :: Int} 
    | NonStudent {name :: String, age :: Int} 
    deriving (Eq, Show)

makeStudent :: IO Person
makeStudent = do putStr "Input name: "
                 n <- getLine
                 putStr "Input age: "
                 a <- getInt
                 return (Student {name = n,age = a})

makeNonStudent :: IO Person
makeNonStudent = do putStr "Input name: "
                    n <- getLine
                    putStr "Input age: "
                    a <- getInt
                    return (NonStudent {name = n,age = a})

price :: [Person] -> Float
price [] = 0.0
price ((Student {name = n,age = a}):xs)
    | a<=18 = price xs
    | otherwise = 160.0 + price xs
price ((NonStudent {name = n,age = a}):xs)
    | a<=60 = 200.0 + price xs
    | otherwise = (200.0 - fromIntegral a)*1.0 + price xs

go' :: [Person] -> IO()
go' xs = do putStr "Student? [Y/N]: "
            choice <- getLine
            if choice == "Y" then
                do p <- makeStudent
                   go' (p:xs)
            else 
                if choice == "N" then
                    do p <- makeNonStudent
                       go' (p:xs)
                else
                    do putStrLn (show xs)
                       putStrLn ("Total price is " ++ show (price xs))

go :: IO()
go = go' []

p2 :: Parser (Char, Char)
p2 = item >>= (\x ->
        item >>= (\y ->
            return (x,y)))

p3 :: Parser (Char, Char)
p3 = P (\inp -> case parse item inp of 
    [] -> []
    [(v,out)] -> (parse (
        P (\inp -> case parse item inp of
            [] -> []
            [(v2,out2)] -> parse ((\y -> return (v,y)) v2) out2)
            )
        ) out)

data Expr = Mul Expr Expr | Div Expr Expr | Val Int deriving (Eq, Show)

eval :: Expr -> Maybe Int 
eval (Val x) = return x 
eval (Mul x y) = do a <- eval x
                    b <- eval y
                    return (a * b)
eval (Div x y) = do a <- eval x 
                    b <- eval y 
                    if b == 0 then 
                        Nothing 
                    else 
                        return (a `div` b)

data Exception a = Raise String | Return a deriving (Eq, Show)

instance Monad Exception where 
    return = Return
    m >>= k = case m of 
        (Return a) -> k a
        (Raise s) -> Raise s

instance Applicative Exception where 
    pure = return 
    (<*>) = ap

instance Functor Exception where 
    fmap = liftM

eval' :: Expr -> Exception Int 
eval' (Val x) = return x 
eval' (Mul x y) = do a <- eval' x
                     b <- eval' y
                     return (a * b)
eval' (Div x y) = do a <- eval' x 
                     b <- eval' y 
                     if b == 0 then 
                        Raise (show a ++ " div 0")
                     else 
                        return (a `div` b)