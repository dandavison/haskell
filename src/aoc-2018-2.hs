import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Map (Map, (!?))


part1 :: Ord a => [[a]] -> Int
part1 lists =
  (countListsWithMultiplicity 2 lists) * (countListsWithMultiplicity 3 lists)


part2 = pairsDifferingAtNPositions 1


getInput = do
  contents <- getContents
  return $ map parseLine (lines contents)


parseLine line = line


countListsWithMultiplicity :: Ord a => Integer -> [[a]] -> Int
countListsWithMultiplicity n lists =
  length [list | list <- lists, hasMultiplicity n list]
  where
    hasMultiplicity n list = any (\(el, count) -> count == n) (Map.toList $ countElements list)


countElements :: Ord a => [a] -> Map a Integer
countElements = countElements' Map.empty


countElements' :: Ord a => Map a Integer -> [a] -> Map a Integer
countElements' counts (x:xs)  = countElements' (increment x counts) xs
  where
    increment x counts = Map.insert x (1 + (fromMaybe 0 (counts !? x))) counts
countElements' counts [] = counts


pairsDifferingAtNPositions :: Eq a => Int -> [[a]] -> [([a], [a])]
pairsDifferingAtNPositions n lists =
  filter (\pair -> nDifferingPositions pair == n) (unorderedPairs lists)


nDifferingPositions :: Eq a => ([a], [a]) -> Int
nDifferingPositions pair = length [(el1, el2) | (el1, el2) <- zip (fst pair) (snd pair), el1 /= el2]


unorderedPairs :: [a] -> [(a, a)]
unorderedPairs xs = [(nth i xs, nth j xs) | i <- [1..n], j <- [1..n], i < j]
  where
    n = length xs


-- 1-based
nth :: Integral n => n -> [a] -> a
nth n xs = head $ (functionPower (n - 1) tail) xs


functionPower :: Integral n => n -> (a -> a) -> (a -> a)
functionPower 0 fn = id
functionPower 1 fn = fn
functionPower n fn = fn . (functionPower (n - 1) fn)


main = do
  input <- getInput
  print $ part1 input
  print $ part2 input
