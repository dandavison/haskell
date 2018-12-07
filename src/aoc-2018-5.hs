import  Data.Char (toLower, toUpper)
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


part1 = length . react

react :: String -> String
react = foldr cons []

cons :: Char -> [Char] -> [Char]
cons x [] = [x]
cons x (y:ys)
  | x `anti` y = ys
  | otherwise = x:y:ys
  where
    anti x y = toLower x == toLower y && x /= y

part2 polymer = minimum $ length . react <$> [filter (not . ((flip elem) [u, toUpper u])) polymer | u <- ['a'..'z']]


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
