import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Map (Map, (!?))


getInput = do
  contents <- getContents
  return $ map parseLine (lines contents)


parseLine line = line


processInput :: Ord a => [[a]] -> Int
processInput lists =
  (countListsWithMultiplicity 2 lists) * (countListsWithMultiplicity 3 lists)


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


main = do
  input <- getInput
  -- part 1
  print $ processInput input
