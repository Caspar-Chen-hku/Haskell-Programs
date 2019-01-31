-- The bind operator (>>=) is used in some places,
-- but they can be replaced by case-of or do-notation.

import Data.Char
import Data.List
import System.IO
import Parsing

-- Expression trees --

-- Problem 1
data Op = Add | Sub | Div
  deriving (Eq, Show)

safeDiv :: [Int] -> [Int] -> Maybe [Int]
safeDiv [] [] = Just []
safeDiv (x:xs) (0:ys) = Nothing
safeDiv (x:xs) (y:ys) = case safeDiv xs ys of
                          Just zs -> Just ((x `div` y) : zs)
                          Nothing -> Nothing
-- safeDiv xs ys >>= \zs -> Just ((x `div` y) : zs)


applyOp :: Op -> Maybe [Int] -> Maybe [Int] -> Maybe [Int]
applyOp Add (Just xs) (Just ys) | length xs == length ys = Just (map (\(x,y) -> x+y) $ zip xs ys)
                                | otherwise              = Nothing

applyOp Sub (Just xs) (Just ys) | length xs == length ys = Just (map (\(x,y) -> x-y) $ zip xs ys)
                                | otherwise              = Nothing

applyOp Div (Just xs) (Just ys) | length xs == length ys = safeDiv xs ys
                                | otherwise              = Nothing

applyOp _ _ _ = Nothing


-- P2
data Expr
  = Bin Op Expr Expr
  | Val [Int]
  | Var String
  deriving (Eq, Show)

type Env = [(String, [Int])]

eval :: Env -> Expr -> Maybe [Int]
eval env (Bin op e1 e2) = applyOp op (eval env e1) (eval env e2)
eval _   (Val xs)       = Just xs
eval env (Var s)        = lookup s env


-- Parsing Expressions --
-- P3
pList :: Parser Expr
pList = p0 +++ p1
  where p0 = do token $ char '['
                n <- integer
                ns <- pSomeInts
                token $ char ']'
                return $ Val (n:ns)
        p1 = do token $ char '['
                token $ char ']'
                return $ Val []

pSomeInts :: Parser [Int]
pSomeInts = p2 +++ return []
  where p2 = do char ','
                n <- integer
                ns <- pSomeInts
                return $ n:ns

-- P4
pExpr :: Parser Expr
pExpr = do t   <- pTerm
           opt <- pOpTerm t
           return opt

-- or pExpr = pTerm >>= pOpTerm

pOpTerm :: Expr -> Parser Expr
pOpTerm e = pOpTerm1 e +++ return e

pOpTerm1 :: Expr -> Parser Expr
pOpTerm1 e = do op <- token $ pOpT
                t  <- pTerm
                opt<- pOpTerm (Bin op e t)
                return opt
  where pOpT = (do char '+'
                   return Add) +++
               (do char '-'
                   return Sub)
-- or pOpT = (char '+' >>= (\_ -> return Add))  +++  (char '-' >>= (\_ -> return Sub))

pTerm :: Parser Expr
pTerm = pFactor >>= pOpFactor

pOpFactor :: Expr -> Parser Expr
pOpFactor e = pOpFactor1 e +++ return e

pOpFactor1 :: Expr -> Parser Expr
pOpFactor1 e = do token $ char ('/')
                  f  <- pFactor
                  opf<- pOpFactor (Bin Div e f)
                  return opf

pFactor :: Parser Expr
pFactor = pE +++ pList
  +++ (do x <- identifier 
          return $ Var x)
  where pE = do _ <- token $ char '('
                e <- pExpr
                _ <- token $ char ')'
                return e

-- The following code shows another way to build these parsers:
{- 
pExpr :: Parser Expr
pExpr = do
  t <- pTerm
  let pOpTerm = do
        op <- symbol "+" +++ symbol "-"
        tm <- pTerm
        return (op, tm)
  ts <- many pOpTerm
  let build t1 (op, t2) = Bin (if op == "+" then Add else Sub) t1 t2
  return $ foldl build t ts


pTerm :: Parser Expr
pTerm = do
  f <- pFactor
  let pOpFactor = do
        op <- symbol "/"
        fc <- pFactor
        return (op, fc)
  fs <- many pOpFactor
  let build f1 (op, f2) = Bin Div f1 f2
  return $ foldl build f fs


pFactor :: Parser Expr
pFactor = pList +++ pVar +++ pParens
  where
    pVar = do
      x <- pName
      return (Var x)
    pParens = do
      _ <- symbol "("
      e <- pExpr
      _ <- symbol ")"
      return e
-}

-- P5
runParser :: Parser a -> String -> Maybe a
runParser parser str =
  case parse parser str of
    []       -> Nothing
    (v, r):_ -> if null r then Just v else Nothing

-- Compilation --
data Instr = IVal [Int] | IBin Op | IVar String
  deriving (Eq, Show)

type Stack = [[Int]]
type Prog = [Instr]


--P6
check :: Expr -> Env -> Bool
check exp env = case check1 exp env of
                  Just _ -> True
                  _      -> False

check1 :: Expr -> Env -> Maybe Int
check1 (Val xs) _         = Just $ length xs
check1 (Var s)  env       = lookup s env >>= (Just . length)
check1 (Bin op e1 e2) env = if check1 e1 env == check1 e2 env then check1 e1 env
  else Nothing

--P7
compile :: Expr -> Env -> Maybe Prog
compile e env = if check e env
                   then Just (compile1 e)
                   else Nothing

compile1 :: Expr -> Prog
compile1 (Bin op e1 e2) = compile1 e2 ++ compile1 e1 ++ [IBin op]
compile1 (Val xs)       = [IVal xs]
compile1 (Var s)        = [IVar s]

--P8
runProg :: Prog -> Env -> Maybe [Int]
runProg p e = case foldl (\mst instr -> (mst >>= runInstr e instr)) (Just []) p of
                Just [n] -> Just n
                _        -> Nothing


runInstr :: Env -> Instr -> Stack -> Maybe Stack
runInstr _ (IVal n) s           = Just $ n:s
runInstr _ (IBin op) (n1:n2:ns) = applyOp op (Just n1) (Just n2) >>= \n -> (Just $ n:ns)
runInstr e (IVar x) s           = lookup x e >>= \n -> Just (n:s)
runInstr _ _ _ = Nothing

-- REPL: Read-Eval-Print Loop --
main :: IO ()
main = do hSetBuffering stdin LineBuffering
          hSetBuffering stdout NoBuffering
          repl []

repl :: Env -> IO ()
repl env = do putStr "\n> "
              line <- getLine
              dispatch env line

quit :: IO ()
quit = return ()

loop :: String -> Env -> IO ()
loop str next = do putStrLn str
                   repl next

--P9
printPair :: (String, [Int]) -> String
printPair (s, ns) =  s ++ " = " ++ (show ns)

printEnv :: Env -> String
printEnv []     = ""
printEnv [x]    = printPair x
printEnv (x:xs) = printPair x ++ "\n" ++ printEnv xs

addPair :: (String, [Int]) -> Env -> Env
addPair (s, n) []           = [(s,n)]
addPair (s, n) ((s',n'):ps) | s == s'   = (s,n) : ps
                            | otherwise = (s',n') : (addPair (s,n) ps)

dispatch :: Env -> String -> IO ()
dispatch e s = case (runParser pCommand s) of
                 Just c  -> impCommand c
                 Nothing -> loop "Error" e
  where impCommand Quit            = quit
        impCommand (Ans exp)         =
          case eval e exp of
            Just n  -> loop ("ans = " ++ show n) e
            Nothing -> loop "Error" e
        impCommand PrintEnv        = loop (printEnv $ sort e) e
        impCommand (AddEnv s exp)  =
          case eval e exp of
            Just n  -> loop (printPair (s,n)) (addPair (s,n) e)
            Nothing -> loop "Error" e



data Command = Quit | PrintEnv | AddEnv String Expr | Ans Expr
pCommand :: Parser Command
pCommand = pQuit +++ pEnv +++ pLet +++ pAns
  where pQuit = do symbol "quit"
                   return Quit
        pEnv  = do symbol "env"
                   return PrintEnv
        pLet  = do symbol "let"
                   s <- identifier
                   token $ char '='
                   e <- pExpr
                   return $ AddEnv s e
        pAns  = do e <- pExpr
                   return $ Ans e
