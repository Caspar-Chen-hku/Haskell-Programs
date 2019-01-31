import Control.Monad

data Expr = Mul Expr Expr | Div Expr Expr | Val Int deriving (Eq, Show)

evalMaybe :: Expr -> Maybe Int
evalMaybe (Val x) = return x
evalMaybe (Mul x y) = 
    do r1 <- evalMaybe x
       r2 <- evalMaybe y
       return (r1*r2)
evalMaybe (Div x y) = 
    do r1 <- evalMaybe x
       r2 <- evalMaybe y
       if r2==0 then
        Nothing
       else
        return (r1 `div` r2)

data Exception a = Raise String | Return a deriving (Eq, Show)

instance Monad Exception where 
    return a = Return a
    m >>= k = case m of
        Raise s -> Raise s
        Return a -> k a

instance Applicative Exception where 
    pure = return 
    (<*>) = ap

instance Functor Exception where 
    fmap = liftM

eval2 :: Expr -> Exception Int
eval2 (Val x) = return x
eval2 (Mul x y) =
    do a <- eval2 x
       b <- eval2 y
       return (a*b)
eval2 (Div x y) = 
    do a <- eval2 x
       b <- eval2 y
       if b == 0 then
        Raise ( show a ++ " Div " ++ show b )
       else
        return (a `div` b)
