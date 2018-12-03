import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Map (Map, (!?))
import Data.List.Split


type Point = (Integer, Integer)
type Interval = (Integer, Integer)
type Rectangle = (Integer, Integer, Integer, Integer)


getInput = do
  contents <- getContents
  return $ map parseLine (lines contents)


parseLine :: String -> Rectangle
parseLine line = (x, y, dx, dy)
  where
    [id, _, coords, dim] = words line
    [x, y] = [(read s :: Integer) | s <- splitOn "," (allButLast $ coords)]
    [dx, dy] = [(read s :: Integer) | s <- splitOn "x" dim]
    allButLast = reverse . tail . reverse


part1 :: [Rectangle] -> Int
part1 rectangles =
  length [ (point, count) | (point, count) <- Map.toList counts, count > 1 ]
  where
    counts = getCounts rectangles


getCounts :: [Rectangle] -> Map Point Integer
getCounts rs = getCounts' Map.empty rs


getCounts' :: Map Point Integer -> [Rectangle] -> Map Point Integer
getCounts' counts [] = counts
getCounts' counts (r:rs) = getCounts' (increment counts (getPoints r)) rs
  where
    increment counts [] = counts
    increment counts (p:ps) = increment (Map.insert p (1 + (fromMaybe 0 (counts !? p))) counts) ps


getPoints :: Rectangle -> [Point]
getPoints r = [(i, j) | i <- [x..(x + dx - 1)], j <- [y..(y + dy - 1)]]
  where
    (x, y, dx, dy) = r


main = do
  rectangles <- getInput
  print $ part1 rectangles
