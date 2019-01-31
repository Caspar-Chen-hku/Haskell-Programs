import Parsing

-- "3+2"

-- expr -> term ('+' expr | \epsilon)
expr :: Parser Int
expr = do t <- term
          (do char '+'
              e <- expr
              return (t + e)) +++ return t

-- term -> factor ('*' term | \epsilon)
term :: Parser Int
term = do f <- factor
          (do char '*'
              t <- term
              return (f * t)) +++ return f

-- factor -> '(' expr ')' | digit
factor :: Parser Int
factor = (do char '('
             e <- expr
             char ')'
             return e) +++ natural -- do {x <- natural; return x}

eval :: String -> Int
eval xs = fst (head (parse expr xs))