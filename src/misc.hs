take' :: Integer -> [a] -> [a]
take' _ [] = []
take' 0 (x:xs) = []
take' n (x:xs) = x : (take' (n - 1) xs)
