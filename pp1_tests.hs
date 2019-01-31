prod :: [Int] -> String -> Int
prod ns xs = product [ x | (x,y) <- xss, y=='y' ]  where xss = zip ns xs

prod' :: [Int] -> String -> Int
prod' (n:ns) (x:xs)
    | x=='y' = n*prod' ns xs
    | otherwise = prod' ns xs
prod' _ _ = 1

charToInt :: Char -> Int
charToInt c
  | c>='0' && c<='9' = read [c] :: Int
  | otherwise = 0

sumeven :: String -> Int
sumeven xs = foldr (\x y -> if even (charToInt x) then charToInt x + y else y) 0 xs

sumeven' :: String -> Int
sumeven' xs = sum [ charToInt x | x<-xs, even (charToInt x) ]

data Move = Go Int | Turn | Dance deriving (Show,Eq)
data Command = Nil | Follow Command Move deriving (Show,Eq)

(|>) :: Command -> Move -> Command
(|>) = Follow

type Position = Int
data Direction = L | R deriving (Show,Eq)
type State = (Position, Direction)

--------question a--------
state :: Move -> State -> State
state (Go x) (p,d)
  | d==R = (p+x,d)
  | otherwise = (p-x,d)
state Turn (p,d)
  | d==R = (p,L)
  | otherwise = (p,R)
state Dance (p,d) = (p,d)

finalstate :: Command -> State -> State
finalstate Nil x = x
finalstate (Follow c m) x = state m (finalstate c x)

same :: Move -> Move -> Int
same (Go _) (Go _) = 1
same Turn Turn = -1
same _ _ = 0

combine :: Move -> Move -> Move
combine (Go x) (Go y) = Go (x+y)

simplify' :: Command -> Command
simplify' Nil = Nil
simplify' (Follow Nil x) = Follow Nil x
simplify' (Follow (Follow x y) z)
  | s == 1 = simplify' (Follow x (combine y z))
  | s == -1 = simplify' x
  | otherwise = Follow (simplify' (Follow x y)) z
  where s = same y z

simplify :: Command -> Command
simplify c
  | r == c = c
  | otherwise = simplify r
  where r = simplify' c
