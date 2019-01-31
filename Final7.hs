import Final3
import System.IO
import Prelude hiding (LT)
import Data.Char

zip' :: [a] -> [b] -> [(a,b)]
zip' xs ys = [ (xs!!n , ys!!n) | n<-[0..l] ] where l = min (length xs) (length ys) - 1

triangle :: Int -> [String]
triangle n = [ if x==y then "*\n" else "*" | x<-[1..n], y<-[1..x] ]

concat' :: [[a]] -> [a]
concat' xss = [ x | xs<-xss, x<-xs]

testParse :: Parser Char
testParse = do item
               item

testparse :: Parser (Char,Char)
testparse = item >>= \x ->
    item >>= \_ ->
        item >>= \y ->
            return (x,y)

data IP = IP Int Int Int Int deriving Show
data LocalTime = LT Int Int Int Int Int Int deriving Show
data Product = Mouse | Keyboard | Monitor | Speakers deriving Show 
data LogEntry = LogEntry LocalTime IP Product deriving Show
type Log = [LogEntry]

parseIP :: Parser IP
parseIP = do x <- integer
             char '.'
             y <- integer
             char '.'
             z <- integer
             char '.'
             m <- integer
             return (IP x y z m)

parseDate :: Parser LocalTime
parseDate = do y<-integer
               char '-'
               m1<-integer
               char '-'
               d<-integer
               many (char ' ')
               h <-integer
               char ':'
               m2 <- integer
               char ':'
               s <- integer 
               return (LT y m1 d h m2 s)

parseProd :: Parser Product
parseProd = (do x<-string "mouse"
                return Mouse) +++
            (do x<-string "keyboard"
                return Keyboard) +++
            (do x<-string "monitor"
                return Monitor) +++
            (do x<-string "speakers"
                return Speakers)

logParser :: Parser Log
logParser = many (
    do t<-parseDate
       many (char ' ')
       ip <- parseIP
       many (char ' ')
       p <- parseProd
       char '\n'
       return (LogEntry t ip p))

s = "2013-06-29 11:32:12 212.141.23.67 mouse\n2013-06-29 11:33:08 212.141.23.67 monitor\n"

singleInt :: Parser Int
singleInt = do x<-sat isDigit
               return (read [x] :: Int)

data Move = Go Int | Turn | Dance deriving Show
data Command = Nil | Follow Command Move deriving Show
(|>) :: Command-> Move-> Command -- infix notation for Follow 
(|>) = Follow

parseCmd :: Parser Command
parseCmd = do x<-parseMove
              xs <- many parseTerm
              return (toCmd (x:xs))

parseMove :: Parser Move
parseMove = (do x<-string "go "
                n<-singleInt
                return (Go n)) +++
            (do string "dance"
                return Dance) +++
            (do string "turn"
                return Turn)

parseTerm :: Parser Move
parseTerm = do string " > "
               parseMove

toCmd :: [Move] -> Command
toCmd [] = Nil
toCmd xs = (toCmd (init xs)) |> (last xs)

manyN :: Int -> Parser a -> Parser [a]
manyN 0 _ = return []
manyN n p = do x<-p
               xs<-manyN (n-1) p
               return (x:xs)

parseElement :: Parser Int
parseElement = (do char ' '
                   return 0) +++
               (do char '*'
                   return 1)

parseLine :: Int -> Parser Int
parseLine n = do xs <- manyN (n+1) parseElement
                 char '\n'
                 return (sum xs)

parseGrid :: (Int,Int) -> Parser Int
parseGrid (x,y) = do xs <- manyN (y+1) (parseLine x)
                     return (sum xs)

testSymbol :: (Char,Char) -> Bool
testSymbol cpair@(a,b)
    | a/=b = test' cpair
    | otherwise = False

test' :: (Char,Char) -> Bool
test' _ = True