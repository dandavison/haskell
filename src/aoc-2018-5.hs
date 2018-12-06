import  Data.Char (isUpper, toLower, toUpper)

getInput = do
  input <- getContents
  return $ trim input


trim :: String -> String
trim = reverse . dropWhile (== '\n') . reverse


part1 polymer = length $ react polymer


react :: String -> String
react xs
  | i == -1 = xs
  | otherwise = react $ (take i xs) ++ (drop (i + 2) xs)
  where
    i = indexOfReactivePair xs


indexOfReactivePair :: String -> Int
indexOfReactivePair [] = -1
indexOfReactivePair (x:xs) = indexOfReactivePair' x 0 xs


indexOfReactivePair' :: Char -> Int -> String -> Int
indexOfReactivePair' _ _ [] = -1
indexOfReactivePair' x i (x':xs)
  | (x' == oppositeCase x) = i
  | otherwise = indexOfReactivePair' x' (i + 1) xs


oppositeCase :: Char -> Char
oppositeCase c = if isUpper c then toLower c else toUpper c


main = do
  input <- getInput
  print $ part1 input
