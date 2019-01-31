greater :: String -> Bool
greater xs = and [ x>='5' || x<'0' | x<-xs]

greater1 :: String -> Bool
greater1 "" = True
greater1 (x:xs)
  | x>='5' || x<'0' = greater1 xs
  | otherwise = False

greater2 :: String -> Bool
greater2 xs = length (filter not
    (foldr (\x y -> if x>='5' then True:y else False:y)
    [] (map (\x -> if x<'0' then '9' else x) xs) )) == 0

--produce :: [a] -> [(a,a)]
--produce xs = [ (xs!!n,xs!!(n+1)) | n<-[0..l], even n] where l = length xs -1

--swap :: [a] -> [a]
--swap xs = concat [ [y,x] | (x,y) <- xss ] where xss = produce xs

swap :: [a] -> [a]
swap xs = concat [ [xs!!(x+1),xs!!x] | x <- [0..l], even x ] where l = length xs - 2

swap1 :: [a] -> [a]
swap1 [] = []
swap1 (x:xs) = [head xs, x] ++ swap1 (tail xs)

type Point = (Int,Int)
data Points = Lines Int Int
  | Columns Int Int
  | Union Points Points
  | Intersection Points Points

inPoints :: Point -> Points -> Bool
inPoints p (Lines a b) = snd p >= a && snd p <= b
inPoints p (Columns a b) = fst p >= a && fst p <= b
inPoints p (Union p1 p2) = inPoints p p1 || inPoints p p2
inPoints p (Intersection p1 p2) = inPoints p p1 && inPoints p p2

showPoint :: Point -> Points -> Char
showPoint p ps
  | inPoints p ps = '*'
  | otherwise = ' '

showPoints :: Point -> Points -> [String]
showPoints p ps = [ [ showPoint (x,y) ps | x<-[0..(fst p)] ] | y<-[0..(snd p)] ]