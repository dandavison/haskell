import  Data.Char (isUpper, toLower, toUpper)
import Test.Hspec (hspec, describe, it, shouldBe)


argmin :: (Eq a, Ord b) => (a -> b) -> [a] -> a
argmin f xs = xmin
  where
    [xmin] = [x | x <- xs, f x == fmin]
    fmin = minimum (f <$> xs)


getInput = do
  input <- getContents
  return $ trim input


trim :: String -> String
trim = reverse . dropWhile (== '\n') . reverse


part1 polymer = length $ react polymer


part2 polymer = minimum $ length . react <$> [filter (not . ((flip elem) [u, toUpper u])) polymer | u <- ['a'..'z']]


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


test = hspec $ do
  describe "test" $ do
    let polymer = "dabAcCaCBAcCcaDA"

    it "part1" $ do
      part1 polymer `shouldBe` 10

    it "part2" $ do
      part2 polymer `shouldBe` 4


main = do
  test
  polymer <- getInput
  print $ part1 polymer
  print $ part2 polymer
