f :: Num a => a -> [a]
f x = [x]

main :: IO ()
main = do
  print $ f 1
  print $ f 'a'
