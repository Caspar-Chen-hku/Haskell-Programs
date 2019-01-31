module A3 where

import           Data.Char
import           Data.List
import           Parsing
import           System.IO

-- Auxiliary functions
position :: Eq a => a -> [a] -> Int
position = go 0
  where
    go :: Eq a => Int -> a -> [a] -> Int
    go _ _ []     = -1
    go i v (x:xs) = if v == x then i else go (i + 1) v xs


intToColumn :: Int -> String
intToColumn 0 = []
intToColumn n =
  intToColumn (m `div` 26) ++ [alphabet !! (m `mod` 26)]
    where
      m = n - 1
      alphabet = ['A'..'Z']


columnToInt :: String -> Int
columnToInt [] = 0
columnToInt (x:xs) =
  first * (26 ^ length xs) + columnToInt xs
    where
      alphabet = ['A'..'Z']
      first = 1 + position x alphabet

-- Expression trees
data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | Val Int
  | Var String
  deriving (Eq, Show)


type Env = [(String, Int)]


lift :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
lift f (Just x) (Just y) = Just (f x y)
lift _ _ _               = Nothing


eval :: Env -> Expr -> Maybe Int
eval env = go
  where
    go (Add a b) = lift (+) (go a) (go b)
    go (Sub a b) = lift (-) (go a) (go b)
    go (Mul a b) = lift (*) (go a) (go b)
    go (Div a b) = lift' div (go a) (go b)
    go (Mod a b) = lift' mod (go a) (go b)
    go (Val v)   = Just v
    go (Var x)   = lookup x env
    lift' _ _ (Just 0) = Nothing
    lift' f a b        = lift f a b

-- Parser of variables
pName :: Parser String
pName = token (ident +++ cellName)
  where
    cellName = do
      cs <- many1 upper
      rs <- many1 digit
      return $ cs ++ rs

-- Parser of expressions
pExpr :: Parser Expr
pExpr = do
  t <- pTerm
  let pOpTerm = do
        op <- symbol "+" +++ symbol "-"
        tm <- pTerm
        return (op, tm)
  ts <- many pOpTerm
  let build t1 (op, t2) = (if op == "+" then Add else Sub) t1 t2
  return $ foldl build t ts


pTerm :: Parser Expr
pTerm = do
  f <- pFactor
  let pOpFactor = do
        op <- symbol "*" +++ symbol "/" +++ symbol "%"
        fc <- pFactor
        return (op, fc)
  fs <- many pOpFactor
  let build f1 (op, f2) = (if op == "*" then Mul else if op == "/" then Div else Mod) f1 f2
  return $ foldl build f fs


pFactor :: Parser Expr
pFactor = pVal +++ pVar +++ pParens
  where
    pVal = do
      n <- natural
      return (Val n)
    pVar = do
      x <- pName
      return (Var x)
    pParens = do
      _ <- symbol "("
      e <- pExpr
      _ <- symbol ")"
      return e

-- Run a given parser
runParser :: Parser a -> String -> Maybe a
runParser parser str =
  case parse parser str of
    []       -> Nothing
    (v, r):_ -> if null r then Just v else Nothing

-- 'concat' with a separator
concatSep :: String -> [String] -> String
concatSep sep = foldr (\w ws -> w ++ sep ++ ws) ""


printTable :: Table -> IO ()
printTable = putStr . strTableToStr . go
  where
    go :: Table -> [[String]]
    go tab = let header = map intToColumn [0..length (head tab)]
                 body = map (\(row, rn) -> show rn : map (maybe "" show) row) (zip tab [1..])
             in header:body

    width :: [[String]] -> [Int]
    width tab = map (\i -> maximum [length (r !! i) | r <- tab]) [0..length (head tab) - 1]

    rowToStr :: [(String, Int)] -> String
    rowToStr = concatSep "|" . map (\(s, w) -> replicate (w - length s) ' ' ++ s)

    strTableToStr :: [[String]] -> String
    strTableToStr tab = let w = width tab in concatSep "\n" $ map (\r -> rowToStr $ zip r w) tab

-- A data type for commands
data Command
  = Quit
  | Delete String
  | PrintTable
  | PrintVars
  | Assign String Expr

-- Parser of commands
pCommand :: Parser Command
pCommand = pQuit +++ pDel +++ pTable +++ pVars +++ pAssign
  where
    pQuit = do
      _ <- symbol "quit"
      return Quit
    pDel = do
      _ <- symbol "del"
      i <- pName
      return $ Delete i
    pTable = do
      _ <- symbol "table"
      return PrintTable
    pVars = do
      _ <- symbol "vars"
      return PrintVars
    pAssign = do
      i <- pName
      _ <- symbol "="
      e <- pExpr
      return $ Assign i e

-- The table type
type Table = [[Maybe Int]]

-- Update the value at a specific position in the table
updateTable :: Int -> Int -> Maybe Int -> Table -> Table
updateTable row col x table = [if ri == row then update r else r | (r, ri) <- zip table [1..]]
  where
    update r = let (left, right) = splitAt (col - 1) r in left ++ (x:tail right)


printBinding :: String -> Int -> IO ()
printBinding v x = putStrLn $ v ++ " = " ++ show x


printEnv :: Env -> IO ()
printEnv [] = return ()
printEnv ((v, x):xs) = do
  printBinding v x
  printEnv xs

-- Add bindings from table to environment
buildEnv :: Env -> Table -> Env
buildEnv env table =
  env ++ [(name rn cn, v) | (row, rn) <- zip table [1..], (cell, cn) <- zip row [1..], cell /= Nothing, let (Just v) = cell]
  where
    name :: Int -> Int -> String
    name r c = intToColumn c ++ show r

-- Work for all the commands
dispatch :: (Env, Table) -> String -> IO ()
dispatch curr@(env, tab) line =
  case runParser pCommand line of
    (Just command) -> go command
    Nothing        -> loop (putStrLn "Error") curr
  where
    maxRow = length tab
    maxCol = length $ head tab

    fullEnv = buildEnv env tab

    update (a, b) xs = (a, b) : filter (\(c, _) -> c /= a) xs

    loop :: IO () -> (Env, Table) -> IO ()
    loop action next = do
      action
      miniExcel next

    updateAndLoop :: String -> Maybe Int -> IO ()
    updateAndLoop v x = do
      let (colStr, rowStr) = span isUpper v
          col = columnToInt colStr
          row = read rowStr
      if row > 0 && row <= maxRow && col > 0 && col <= maxCol
        then do
          case x of (Just x') -> printBinding v x'
                    Nothing   -> putStrLn $ "Deleted " ++ v
          miniExcel (env, updateTable row col x tab)
        else loop (putStrLn "Error") curr

    go :: Command -> IO ()
    go Quit = return ()
    go (Delete v)
      | isLower $ head v =
        case lookup v env of
          (Just x) -> loop (putStrLn $ "Deleted " ++ v) (delete (v, x) env, tab)
          Nothing -> loop (putStrLn "Error") curr
      | otherwise = updateAndLoop v Nothing
    go PrintTable = loop (printTable tab) curr
    go PrintVars = loop (printEnv $ sort env) curr
    go (Assign v expr) =
      case eval fullEnv expr of
        (Just x) | isLower $ head v -> loop (printBinding v x) (update (v, x) env, tab)
                 | otherwise -> updateAndLoop v (Just x)
        Nothing -> loop (putStrLn "Error") curr

-- Print the prompt, call 'dispatch' with a input line
miniExcel :: (Env, Table) -> IO ()
miniExcel env = do
  putStr "\n> "
  line <- getLine
  dispatch env line


main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  row <- askInt "Number of rows: "
  col <- askInt "Number of columns: "
  miniExcel ([], replicate row (replicate col Nothing))
  where
    askInt :: String -> IO Int
    askInt s = do
      putStr s
      line <- getLine
      case runParser natural line of
        (Just x) | x > 0 -> return x
        _        -> askInt s
