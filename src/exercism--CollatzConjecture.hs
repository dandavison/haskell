module CollatzConjecture (collatz) where


collatz :: Integer -> Maybe Integer
collatz n
  | n >= 1 = firstIndexWhere (== 1) (collatzSequence n)
  | otherwise = Nothing


collatzSequence :: Integer -> [Integer]
collatzSequence n = n : (collatzSequence next)
  where
    next = if n `mod` 2 == 0
           then n `div` 2
           else 3 * n + 1


firstIndexWhere :: Num a => (a -> Bool) -> [a] -> Maybe Integer
firstIndexWhere = firstIndexWhere' 0
  where
    firstIndexWhere' _ _ [] = Nothing
    firstIndexWhere' offset f (x:xs) = if f x
                                       then Just offset
                                       else firstIndexWhere' (offset + 1) f xs

