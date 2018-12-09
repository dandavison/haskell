take' :: Integer -> [a] -> [a]
take' _ [] = []
take' 0 (x:xs) = []
take' n (x:xs) = x : (take' (n - 1) xs)


length' :: [a] -> Integer
length' xs = toInteger $ length xs

take' :: Integer -> [a] -> [a]
take' n = take' (fromInteger n)

drop' :: Integer -> [a] -> [a]
drop' n = drop (fromInteger n)
