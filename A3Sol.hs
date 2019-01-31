
import Prelude hiding (reverse, (++), length, foldr, Maybe(..), return, (>>=))
import Language.Haskell.Liquid.Equational

----------------------------------------
-- Define List
----------------------------------------

{-@ LIQUID "--exact-data-cons" @-}
{-@ LIQUID "--higherorder" @-}

data List a = Nil | Cons a (List a)

{-@ measure length               @-}
{-@ length      :: List a -> Nat @-}
length :: List a -> Int
length Nil        = 0
length (Cons x xs) = 1 + length xs

{-@ data List [length] a = Nil | Cons {hd :: a, tl :: List a} @-}

{-@ infix   ++ @-}
{-@ ++      :: List a -> List a -> List a @-}
{-@ reflect ++ @-}
(++) :: List a -> List a -> List a
(++) Nil ys        = ys
(++) (Cons x xs) ys = Cons x (xs ++ ys)

{-@ reverse :: List a -> List a @-}
{-@ reflect reverse @-}
reverse :: List a -> List a
reverse Nil        = Nil
reverse (Cons x xs) = reverse xs ++ (Cons x Nil)

{-@ reflect foldr @-}
foldr :: (a -> b -> b) -> b -> List a -> b
foldr _ v Nil = v
foldr f v (Cons x xs) = f x (foldr f v xs)

----------------------------------------
-- Example
----------------------------------------

{-@ lengthConcat :: xs:List a -> ys:List a -> {length (xs ++ ys) == length xs + length ys} @-}
lengthConcat :: List a -> List a -> Proof
lengthConcat Nil ys
  = length (Nil ++ ys)
  ==. length ys
  ==. 0 + length ys
  ==. length Nil + length ys
  *** QED
lengthConcat (Cons x xs) ys
  = length (Cons x xs ++ ys)
  ==. length (Cons x (xs ++ ys))
  ==. 1 + length (xs ++ ys)
  ==. 1 + length xs + length ys ? lengthConcat xs ys
  ==. length (Cons x xs) + length ys
  *** QED
  
----------------------------------------
-- Question 1
-- uncomment the following line and finish the undefined part
----------------------------------------

{-@ lengthReverse :: xs:List a -> {length (reverse xs) == length xs}@-}
lengthReverse :: List a -> Proof
lengthReverse l = undefined
  
----------------------------------------
-- Question 2
-- uncomment the following line and finish the undefined part
----------------------------------------

{-@ twiceReverse :: xs:List a -> {reverse (reverse xs) == xs}@-}
twiceReverse :: List a -> Proof
twiceReverse l = undefined


----------------------------------------
-- Question 3
-- you may or may not need to use beta_application,
-- which essentially states that f x == (\a -> f a) x
----------------------------------------

{-@ beta_application :: bd:b -> f:(a -> {bd':b | bd' == bd}) -> x:a -> {f x == bd } @-}
beta_application :: b -> (a -> b) -> a -> Proof
beta_application bd f x
  = f x ==. bd *** QED


data Maybe a = Nothing | Just a

{-@ reflect return  @-}
return :: a -> Maybe a
return a = Just a

{-@ infix   >>= @-}
{-@ reflect >>= @-}
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>=) Nothing _ = Nothing
(>>=) (Just a) f = f a

{-@ left_identity :: v:a -> f:(a -> Maybe b) -> {(return v) >>= f == f v} @-}
left_identity :: a -> (a -> Maybe b) -> Proof
left_identity a f = undefined

{-@ right_identity :: m:(Maybe a) -> {m >>= return == m} @-}
right_identity :: (Maybe a) -> Proof
right_identity a = undefined

{-@ associativity :: m:(Maybe a) -> k:(a -> Maybe b) -> h:(b -> Maybe c) -> {m >>= (\x:a -> k x >>= h) == (m >>= k) >>= h} @-}
associativity :: (Maybe a) -> (a -> Maybe b) -> (b -> Maybe c) -> Proof
associativity a k h = undefined
