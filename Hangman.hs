import Parsing
import System.IO

hangman :: IO ()
hangman = 
    do putStrLn "Think of a word: "
       word <- sgetLine  -- secret getLine
       if word == "" then
          do putStrLn "Enter a valid word!"
             hangman
       else
          do putStrLn "Try to guess it:"
             play word

--get a character without showing it
getCh :: IO Char
getCh = do hSetEcho stdin False  -- Turn off echo
           x <- getChar
           hSetEcho stdin True  -- Turn on echo
           return x

--secretly get a line of string
--without showing it explicitly
sgetLine :: IO String
sgetLine = do x <- getCh
              if x /= '\n' then
                do putChar '-'
                   xs <- sgetLine 
                   return (x:xs) 
              else
                return ""

play :: String -> IO ()
play word = do guess <- getLine
               if (guess == word) then
                putStrLn "You got it!" else
                    do putStrLn (show (length word))
                       putStrLn (match word guess)
                       play word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-'  | x <- xs, x /= '\n']
