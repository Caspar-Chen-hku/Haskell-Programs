import Prelude hiding (getLine, putStr)

act :: IO (Char,Char) 
act  = do x <- getChar           
          getChar           
          y <- getChar           
          return (x,y)

getLine :: IO String 
getLine  = do x <- getChar               
              if x == '\n' then                  
                return []               
              else                  
                do xs <- getLine                     
                   return (x:xs)


putStr :: String -> IO()
putStr [] = return []
putStr (x:xs) = return (putChar x : putStr xs) 