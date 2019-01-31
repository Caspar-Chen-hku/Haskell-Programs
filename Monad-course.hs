import Parsing

instance Functor Option
instance Applicative Option

a :: IO (Char,Char) 
a  = do x <- getChar            
        getChar            
        y <- getChar            
        return (x,y)

a' :: Parser (Char, Char)
a' = item >>= (\x ->
        (item >>= (\_ ->
            (item >>= (\y ->
                return (x,y)
                )
            )
            )
        )
    )

--Option is just Maybe with another name:
--  None -> represents a program with exceptions
--  Some -> without exception
data Option a = None | Some a deriving Show

instance Monad Option where
    return x = Some x
    None >>= f = None
    (Some x) >>= f = f x

sdiv :: Int -> Int -> Option Int
sdiv n 0 = None
sdiv n m = Some (n `div` m)

p4 :: Option Int
p4 = do x <- 4 `sdiv` 2
        return x

