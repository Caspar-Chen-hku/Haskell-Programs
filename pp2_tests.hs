--second :: Char -> String -> String
--second c xs = [ (\n -> if even n then c else xs!!n) n | n<-[0..l] ] where l = length xs - 1

second :: Char -> String -> String
second c xs = [ if even x then c else y | (x,y)<-zipped] where 
  zipped = zip [0..(length xs - 1)] xs

secondRec :: Char -> Int -> String -> String
secondRec _ _ "" = ""
secondRec c n (x:xs)
  | even n = c : s
  | otherwise = x : s where
    s = secondRec c (n+1) xs

second' :: Char -> String -> String
second' c = secondRec c 0

secondh :: Char -> String -> String
secondh c xs = map (\(index,item) -> if even index then c else item) (zip [0..l] xs) where l = length xs -1

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

trace :: Command -> State -> [State]
trace (Nil) s  = [s]
trace (Follow c m) s = traced ++ [state m (last traced)] where traced = trace c s

pos :: State -> [State] -> Bool
pos _ [] = False
pos s (x:xs) = fst s == fst x || pos s xs

dancify :: Command -> Command
dancify Nil = Nil
dancify (Follow c m)
    | m==Dance = prior
    | pos (last traced) (init traced) = Follow prior Dance
    | otherwise = prior 
    where 
        prior = Follow (dancify c) m
        traced = trace (Follow c m) (0,L)