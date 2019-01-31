
-- During the Tutorial, I have written Review, Question1, 2 and 5 in Liquid Haskell syntax.
-- But I have omitted many details so they are not going to be a valid LH program.
-- LH syntax is not required for equational reasoning problems in exams.
-- Here the important point is to understand how to do equational reasoning based in inductions.
-- Question 3 and 4 are in normal paper-and-pencil style, and this is what you should do in exams.

================================== Review =======================================

Prove:

map f (map g xs) = map (f . g) xs

Proof:

Induction on xs

Base case: xs = []

map f (map g [])
-- {xs = []}
= map f (map g [])
-- {Definition of map}
==. []
-- {Definition of map}
==. map (f . g) []
-- {xs = []}
-- ==. map (f . g) xs 
*** QED


Inductive case: xs = y:ys

map_dot
known: map f (map g ys) = map (f . g) ys

map_dot f (map g (y:ys))
-- {xs = y:ys}
= map f (map g (y:ys))
-- {Definition of map}
==. map f (g y : map g ys)
-- {Definition of map}
==. f (g y) : map f (map g ys)
-- {Induction hypothesis}
==. f (g y) : map (f . g) ys  ? map_dot f (map g ys)
-- {Definition of map and .}
==. map (f . g) (y:ys)
-- {xs = (y:ys)}
-- ==. map (f . g) xs
*** QED



================================== Question 1 =======================================

Prove:

all_replicate : all (==x) (replicate n x) = True

Induction on n >=0

Base n = 0

all_replicate (==x) (replicate 0 x)
= all (==x) (replicate 0 x)
-- definition of replicate
==. all (==x) []
-- definition of all
==. True
*** QED

Inductive n = 1 + m

Inductive hypothesis:
  all_replicate : all (==x) (replicate m x) = True

all_replicate (==x) (replicate (1 + m) x)
= all (==x) (replicate (1 + m) x)
-- definition of replicate
==. all (==x) (x : replicate m x)
-- definition of all
==. x == x && all (==x) (replicate m x)
-- x == x
==. True && all (==x) (replicate m x)
-- definition of &&
==. all (==x) (replicate m x) ? all_replicate (==x) (replicate m x)
-- inductive hypothesis
==. True
*** QEd

================================== Question 2 =======================================

-- PART 1

Prove:
  append_nil (xs) -> xs ++ [] = xs

Induction on xs

Base Case : xs = []

xs ++ []
= [] ++ []
-- definition of ++
==. []
-- xs = []
*** QED

Inductive Case : xs = y : ys

Inductive hypothesis:
  ys ++ [] = ys

xs ++ []
= (y : ys) ++ []
-- definition of ++
==. y : (ys ++ [])
-- inductive hypothesis
==. y : ys    ? append_nil ys
-- xs = y : ys
*** QED


-- PART 2

Prove:
  append_assoc : xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

Induction on xs

Base Case : xs = []

append_assoc
=. [] ++ (ys ++ zs)
-- definition of ++
==. ys ++ zs
-- definition of ++
==. ([] ++ ys) ++ zs
-- xs = []
*** QED

Inductive Case : xs = a : as

((a:as) ++ ys) ++ zs

append_assoc
=. (a:as) ++ (ys ++ zs)
-- definition of ++
==. a : (as ++ (ys ++ zs))
-- inductive hypothesis
==. a : ((as ++ ys) ++ zs) ? append_assoc _ _
-- definition of ++
==. (a : (as ++ ys)) ++ ys
-- definition of ++
==. ((a:as) ++ ys) ++zs
-- xs = a : as
*** QED

================================== Question 3 =======================================

Prove

take n xs ++ drop n xs = xs

Proof:

Simultaneous induction on n and xs

Base case: n = 0

take n xs ++ drop n xs
= {n = 0}
take 0 xs ++ drop 0 xs
= {Definition of take and drop}
[] ++ xs
= {Definition of ++}
xs

Base case: n = m + 1, xs = []

take n xs ++ drop n xs
= {n = m + 1, xs = []}
take (m + 1) [] ++ drop (m + 1) []
= {Definition of take and drop}
[] ++ []
= {Definition of ++}
[]
= {xs = []}
xs

Inductive case: n = m + 1, xs = y:ys

take n xs ++ drop n xs
= {n = m + 1, xs = y:ys}
take (m + 1) (y:ys) ++ drop (m + 1) (y:ys)
= {Definition of take and drop}
(y : take m ys) ++ (drop m ys)
= {Definition of ++}
y : (take m ys ++ drop m ys)
= {Induction hypothesis}
y : ys
= {xs = y:ys}
= xs

================================== Question 4 =======================================

> data Tree = Leaf Int | Node Tree Tree

First we define two functions

> leaves (Leaf _)   = 1
> leaves (Node l r) = leaves l + leaves r
>
> nodes (Leaf _)    = 0
> nodes (Node l r) = 1 + nodes l + nodes r

Prove

nodes t + 1 = leaves t

Proof:

Induction on t

Base case: t = Leaf n

nodes t + 1
= {t = Leaf n}
nodes (Leaf n) + 1
= {Definition of nodes}
0 + 1
= {Simplification}
1
= {Definition of leaves}
leaves (Leaf n)
= {t = Leaf n}
levaes t

Inductive case: t = Node l r

nodes t + 1
= {t = Node l t}
nodes (Node l r) + 1
= {Definition of notes}
(1 + nodes l) + nodes r + 1
= {Simplification}
(nodes l + 1) + (nodes r + 1)
= {Induction hypothesis}
leaves l + leaves r
= {Definition of leaves}
leaves (Node l r)
= {t = Node l r}
leaves t


================================== Question 5 =======================================

data Expr = Val Int | Add Expr Expr

type Stack = [Int]
type Code = [Op]
data Op = PUSH Int | ADD

comp :: Expr -> Code
comp (Val n)   = [PUSH n]
comp (Add x y) = comp x ++comp y ++ [ADD]

Lemma lemma:
  comp' e c = comp e ++ c

Induction e

Base Case: e = Val n

comp' (Val n) c
-- according to lemma
= comp (Val n) ++ c
-- definition of comp
==. [PUSH n] ++ c
-- definition of ++
==. PUSH n : c

Inductive Case:

  e = Add e1 e2

Inductive Hypothesis
  comp' (e1) c = comp e1 ++ c
  comp' (e2) c = comp e1 ++ c

comp' (Add e1 e2) c
-- according to lemma
= comp (Add e1 e2) c
-- definition of comp
==. (comp e1 ++ comp e2) ++ [ADD] ++ c
-- associativity
==. comp e1 ++ (comp e2 ++ [ADD] ++ c)
-- associativity
==. comp e1 ++ (comp e2 ++ ([ADD] ++ c))
-- definition of ++
==. comp e1 ++ (comp e2 ++ (ADD :c))
-- inductive hypothesis
==. comp e1 ++ (comp' (e2) (ADD : c))
-- inductive hypothesis
==. comp' e1 (comp' e2 (ADD : c))

-- FULL definition of comp'

comp' (Val n) c = PUSH n : c
comp' (Add e1 e2) c = ==. comp' e1 (comp' e2 (Add : c))
