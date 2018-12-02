import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Map (Map, (!?))


getInput = do
  contents <- getContents
  return $ map parseLine (lines contents)


parseLine line = line


processInput input =
  (Map.toList . tabulate) <$> input


tabulate :: Ord a => [a] -> Map a Integer
tabulate = tabulate' Map.empty


tabulate' :: Ord a => Map a Integer -> [a] -> Map a Integer
tabulate' counts (x:xs)  = tabulate' (increment x counts) xs
  where
    increment x counts = Map.insert x (1 + (fromMaybe 0 (counts !? x))) counts
tabulate' counts [] = counts


main = do
  input <- getInput
  print $ processInput (take 1 input)
