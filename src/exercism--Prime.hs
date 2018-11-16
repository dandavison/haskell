module Prime (nth) where


isPrime :: Integer -> Bool
isPrime i = all (\j -> i `rem` j /= 0) [2..maxPossibleFactor]
  where
    maxPossibleFactor = floor $ sqrt $ fromIntegral i


nth :: Int -> Maybe Integer
nth n = if n > 0
        then Just $ (filter isPrime [1..]) !! n
        else Nothing
