sumOdds :: [Int] -> Int
sumOdds = sum . filter odd . filter (>0)

f :: [Int] -> Bool
f = (>100) . sum . map (\x -> 1 + x*2) . filter even

--let f = negate . sum

data Tree a = Leaf | Node (Tree a) a (Tree a) 
    deriving (Eq, Ord)

t = Node (Node Leaf 2 Leaf ) 4 Leaf

numOfNode :: Tree a -> Int
numOfNode Leaf = 1
numOfNode (Node a b c) = numOfNode a + numOfNode c

balanced :: Tree a -> Bool 
balanced Leaf = True
balanced (Node a b c) = balanced a && balanced c && abs x <= 1 where
    x = numOfNode a - numOfNode c

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node a b c) = Node (mapTree f a) (f b) (mapTree f c)

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree f init Leaf = init
foldTree f init (Node a b c) = f b (foldTree f init a) (foldTree f init c)

data Expr = Val Int
    | Add Expr Expr
    | Mul Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

e = Add (Val 1) (Mul (Val 2) (Val 3))

type Unary a = Int -> a
type Binary a = a -> a -> a

fold :: Unary a -> Binary a -> Binary a -> Expr -> a
fold v _ _ (Val n) = v n
fold v a m (Add x y) = a (fold v a m x) (fold v a m y)
fold v a m (Mul x y) = m (fold v a m x) (fold v a m y)

binary :: String -> String -> String -> String 
binary op x y = "(" ++ x ++ " " ++ op ++ " " ++ y ++ ")"

printTree :: Expr -> String 
printTree (Val n) = show n 
printTree (Add x y) = binary "+" (printTree x) (printTree y) 
printTree (Mul x y) = binary "*" (printTree x) (printTree y)

--question 7
collect :: Expr -> [Int]
collect = fold (\x -> [x]) (\x y -> x ++ y) (\x y -> x ++ y)

evalPrint :: Expr -> (Int, String)
evalPrint = fold (\x -> (x, show x)) (\x y -> ( ( fst x + fst y ), ( "(" ++ snd x ++ " + " ++ snd y ++ ")") ) )
    (\x y -> ( ( fst x * fst y ), ( "(" ++ snd x ++ " * " ++ snd y ++ ")") ) )
