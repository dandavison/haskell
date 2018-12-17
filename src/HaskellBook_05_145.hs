f :: [a] -> [a] -> [a]
f = undefined

myConcat :: [Char] -> [Char]
myConcat x = x `f` " yo"


(<) :: Ord a => a -> a -> Bool
(<) = undefined

myAlph :: Char -> Bool
myAlph x = x Main.< 'z'

main = do
  putStrLn "Done"
