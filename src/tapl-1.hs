data Term
  = TmTrue
  | TmFalse
  | TmIfThenElse Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Eq, Show)


reduce :: Term -> Term
reduce TmTrue = TmTrue
reduce TmFalse = TmFalse
reduce TmZero = TmZero
reduce (TmIfThenElse t1 t2 t3) = if (reduce t1) == TmTrue then t2 else t3
reduce (TmSucc (TmPred t)) = t
reduce (TmPred (TmSucc t)) = t
reduce (TmSucc t) = TmSucc (reduce t)
reduce (TmPred t) = TmPred (reduce t)
reduce (TmIsZero t) = if (reduce t) == TmZero then TmTrue else TmFalse


main :: IO ()
main = do

  putStr "\n"
  putStr "reduce (TmPred (TmSucc TmZero)\n"
  print $ reduce (TmPred (TmSucc TmZero))

  putStr "\n"
  putStr "reduce (TmIfThenElse (TmIsZero (TmPred (TmSucc TmZero))) TmTrue TmFalse)\n"
  print $ reduce (TmIfThenElse (TmIsZero (TmPred (TmSucc TmZero))) TmTrue TmFalse)

