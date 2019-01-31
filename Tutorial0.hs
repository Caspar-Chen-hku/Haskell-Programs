--take (2*2) [1..9], remember the parenthesis is necessary


module Tutorial0 where
import Test.QuickCheck

absolute :: Real a => a -> a
absolute x = if x < 0 then -x else x

prop_absolute :: Real a => a -> a -> Bool
prop_absolute a b = (absolute a) * (absolute b) == absolute (a*b)

prop_absolute1 :: Real a => a -> a -> Bool
prop_absolute1 x y = absolute x + absolute y >= absolute (x+y)

prop_reverse :: Eq a => [a] -> Bool
prop_reverse xs = reverse (reverse xs) == xs
