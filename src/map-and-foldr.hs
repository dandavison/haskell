map' :: (a -> b) -> [a] -> [b]
map' f (x:xs) = f x : map' f xs
map' f [] = []

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f e (x:xs) = f x $ foldr' f e xs
foldr' f e [] = e

main :: IO ()
main = do
  putStr "map' (+ 1) [1, 2, 3] = "
  print $ map' (+ 1) [1, 2, 3]

  putStr "foldr' (+) 0 [1, 2, 3] = "
  print $ foldr' (+) 0 [1, 2, 3]
