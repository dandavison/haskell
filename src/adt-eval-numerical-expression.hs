data Expression = Number Int
                | Add Expression Expression
                | Minus Expression Expression
                | Mult Expression Expression
--                | Divide Expression Expression


eval :: Expression -> Int
eval (Number n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Minus e1 e2) = (eval e1) - (eval e2)
eval (Mult e1 e2) = (eval e1) * (eval e2)
-- eval (Divide e1 e2) = (eval e1) / (eval e2)


main :: IO ()
main = do
  let expr = Mult (Add (Number 4) (Minus (Number 0) (Number 1))) (Number 2)
  print $ eval expr
