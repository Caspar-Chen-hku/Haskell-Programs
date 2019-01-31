module Tutorial3 where

sumOdds :: [Int] -> Int
sumOdds = sum . (filter odd) . (filter (> 0))

f :: [Int] -> Bool
f = (> 100) . sum . map ((+ 1) . (* 2)) . (filter even)

data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving (Eq, Ord)
-- Eq : equivalence test, for example, `==`
-- Ord: compare order, for example, `<`, `>`

-- Show: I just want to print the tree
-- No worry. learn typeclass later
instance (Show a) => Show (Tree a) where
  show Leaf = "leaf"
  show (Node l v r) = "(" ++ show l ++ ", " ++ show v ++ ", " ++ show r ++ ")"

t = Node (Node Leaf 2 Leaf ) 4 Leaf

balanced :: Tree a -> Bool
balanced tree = let (bl, _) = balanced' tree in bl

balanced' :: Tree a -> (Bool, Int)
balanced' Leaf = (True, 1)
balanced' (Node l v r) = (bcurent, cl + cr)
  where bcurent = bl && br && abs (cl - cr) <= 1
        (bl, cl) = balanced' l
        (br, cr) = balanced' r

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node l v r) = Node (mapTree f l) (f v) (mapTree f r)

--a little bit hard to understand!
--fold from right to left
foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree f v Leaf = v
foldTree f v (Node l a r) = foldTree f (f a (foldTree f v r)) l

type Unary a = Int -> a
type Binary a = a -> a -> a

data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr
          deriving Show


e = Add (Mul (Val 2) (Val 3)) (Add (Mul (Val 4) (Val 5)) (Val 6))

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

printTree' = fold show (binary "+") (binary "*")

collect :: Expr -> [Int]
collect = fold (\x -> [x]) (++) (++)

evalPrint :: Expr -> (Int, String)
evalPrint = fold val add mul
  where val = \n -> (n, show n)
        add (i1, s1) (i2, s2) = (i1 + i2, binary "+" s1 s2)
        mul (i1, s1) (i2, s2) = (i1 * i2, binary "*" s1 s2)
