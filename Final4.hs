import System.IO
import Final3

instance Functor Option
instance Applicative Option

alternate :: IO (Char, Char)
alternate = do x <- getChar
               getChar
               y <- getChar
               return (x,y)

getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
                return ""
              else
                do xs <- getLine'
                   return (x:xs)

putStr' :: String -> IO ()
putStr' "" = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

putStrLn' :: String -> IO()
putStrLn' s = do putStr' s
                 putChar '\n'

strlen :: IO Int
strlen = do putStr "Enter a string: "
            x <- getLine
            return (length x)

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                return ""
              else
                do putChar '-'
                   xs <- sgetLine
                   return (x:xs)

hangman :: IO ()
hangman =
    do putStrLn "Think of a word:"
       word <- sgetLine
       putStrLn "Try to guess it:"
       play word

play :: String -> IO ()
play word = do guess <- getLine
               let matched = match guess word in
                if matched == word then
                    putStrLn "Congrats! You've guessed the right word!"
                else
                    do putStrLn matched
                       putStrLn "Try to guess again!"
                       play word

match :: String -> String -> String
match "" ys = map (\_ -> '-') ys
match xs "" = ""
match (x:xs) (y:ys)
    | x==y = x : match xs ys
    | otherwise = '-' : match xs ys

printBoardLn :: Int -> IO ()
printBoardLn 0 = putChar '\n'
printBoardLn n = do putChar '*'
                    printBoardLn (n-1)

printBoard :: [(Int,Int)] -> IO ()
printBoard [] = return ()
printBoard (x:xs) = let (index,num) = x in
                        do putStr (show (index+1) ++ ": ")
                           printBoardLn num
                           printBoard xs

nim :: IO ()
nim = let b = zip [0..4] [5,4,3,2,1] in
        do printBoard b
           playNim 0 b

readNum :: IO Int
readNum = do x <- getLine
             return (read x :: Int)

updateBoard :: Int -> Int -> [(Int,Int)] -> [(Int,Int)]
updateBoard r n board = zip indices (take r xs ++ [ xs!!r -n ] ++ drop (r+1) xs) where (indices,xs) = unzip board

playNim :: Int -> [(Int,Int)] -> IO ()
playNim n xs = do putStrLn ("Player " ++ (show (n+1)))
                  putStr "Select a row to remove: "
                  r <- readNum
                  if r<1 || r>5 then
                    do putStrLn "incorrect input of row number"
                       playNim n xs
                  else
                    do putStr "Number of stars to remove: "
                       star <- readNum
                       if star<1 || star> snd (xs!!(r-1)) then
                        do putStrLn "incorrect input of start number"
                           playNim n xs
                       else
                        let updated = updateBoard (r-1) star xs in
                            if updated == zip [0..4] [0,0,0,0,0] then
                                putStrLn "Congrats! You wins!"
                            else
                                do printBoard updated
                                   playNim ((n+1) `mod` 2) updated

alternate' :: Parser (Char,Char)
alternate' = item >>= (\x ->
    item >>= (\_ ->
        item >>= (\y ->
            return (x,y))))

data Option a = None | Some a deriving Show

instance Monad Option where
    return v = Some v
    p >>= f = case p of
        None -> None
        Some v -> f v

sdiv :: Option Int -> Option Int -> Option Int
sdiv _ (Some 0) = None
sdiv (Some x) (Some y) = Some (x `div` y)
sdiv _ _ = None

sadd :: Option Int -> Option Int -> Option Int
sadd (Some x) (Some y) = (Some (x+y))
sadd _ _ = None