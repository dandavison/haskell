import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Map (Map, (!?))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List.Split


type Point = (Integer, Integer)
type Interval = (Integer, Integer)
type Rectangle = (Integer, Integer, Integer, Integer, Integer)


getInput = do
  contents <- getContents
  return $ map parseLine (lines contents)


parseLine :: String -> Rectangle
parseLine line = (id, x, y, dx, dy)
  where
    [rawID, _, coords, dim] = words line
    id = read (tail rawID) :: Integer
    [x, y] = [(read s :: Integer) | s <- splitOn "," (allButLast $ coords)]
    [dx, dy] = [(read s :: Integer) | s <- splitOn "x" dim]
    allButLast = reverse . tail . reverse


part1 :: [Rectangle] -> Int
part1 rectangles =
  length [ (point, ids) | (point, ids) <- Map.toList counts, length ids > 1 ]
  where
    counts = getCounts rectangles


part2 :: [Rectangle] -> Set Integer
part2 rectangles = Set.difference ids idsInOverlaps
  where
    counts = getCounts rectangles
    ids = foldr Set.union Set.empty
      (map Set.fromList [ids | (point, ids) <- Map.toList counts])
    idsInOverlaps = foldr Set.union Set.empty
      (map Set.fromList [ids | (point, ids) <- Map.toList counts, length ids > 1])


getCounts :: [Rectangle] -> Map Point [Integer]
getCounts rs = getCounts' Map.empty rs


getCounts' :: Map Point [Integer] -> [Rectangle] -> Map Point [Integer]
getCounts' counts [] = counts
getCounts' counts (r:rs) = getCounts' (increment counts (getPoints r)) rs
  where
    increment counts [] = counts
    increment counts (p:ps) = increment (Map.insert p (id : (fromMaybe [] (counts !? p))) counts) ps
    (id, _, _, _, _) = r


getPoints :: Rectangle -> [Point]
getPoints r = [(i, j) | i <- [x..(x + dx - 1)], j <- [y..(y + dy - 1)]]
  where
    (_, x, y, dx, dy) = r


main = do
  rectangles <- getInput
  print $ part1 rectangles
  print $ part2 rectangles
