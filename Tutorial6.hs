data Tree = Leaf Int | Node Tree Tree

t :: Tree
t = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))

numNodes :: Tree -> Int
numNodes (Leaf _) = 1
numNodes (Node t1 t2) = numNodes t1 + numNodes t2 + 1

numLeaves :: Tree -> Int
numLeaves (Leaf _) = 1
numLeaves (Node t1 t2) = numLeaves t1 + numLeaves t2

data Expr = Val Int | Add Expr Expr deriving Show

e :: Expr
e = Add (Val 1) (Add (Val 2) (Val 3))

eval :: Expr -> Int 
eval (Val n) = n 
eval (Add x y) = eval x + eval y

type Stack = [Int] 
type Code = [Op] 
data Op = PUSH Int | ADD deriving Show

exec :: Code -> Stack -> Stack 
exec [] s = s 
exec (PUSH n:c) s = exec c (n:s) 
exec (ADD:c) (m:n:s) = exec c (n+m:s)

comp :: Expr -> Code 
comp (Val n) = [PUSH n] 
comp (Add x y) = comp x ++comp y ++ [ADD]

comp' :: Expr -> Code -> Code
comp' (Val n) c = (PUSH n) : c
comp' (Add x y) c = comp' x [] ++ comp' y [] ++ [ADD] ++ c