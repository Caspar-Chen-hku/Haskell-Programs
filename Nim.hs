import Parsing
import System.IO

nim :: IO ()
nim = 
    do putStrLn "The game of Nim begins!"
       play 0 [5,4,3,2,1]

getInt :: IO Int
getInt = do x <- getLine
            return (read x :: Int)

printBoard :: [Int] -> IO()
printBoard [] = putStr ""
printBoard (x:xs) = do printLn x
                       printBoard xs

printLn :: Int -> IO()
printLn 0 = putStrLn ""
printLn x = do putStr "*" 
               printLn (x-1)

newBoard :: [Int] -> Int -> [Int]
newBoard (x:xs) n
  | x==n = xs
  | otherwise = x-n : xs

play :: Int -> [Int] -> IO ()
play n xs = do printBoard xs
               putStr ("player " ++ show (n+1) ++ " please enter an integer from 1 to " ++ show (head xs) ++ ": ")
               x <- getInt
               if x<=0 || x>head xs then
                do putStrLn "please enter a valid integer!"
                   play n xs
               else
                let newxs = newBoard xs x in
                  if newxs == [] then
                    putStrLn ("player " ++ show (n+1) ++ " wins!")
                  else
                    play ( (n+1) `mod` 2 ) newxs
