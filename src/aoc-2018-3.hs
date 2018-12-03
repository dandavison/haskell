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
  length [ (point, count) | (point, count) <- Map.toList overlapCounts, count > 1 ]
  where
    overlapCounts = getOverlapCounts (unorderedPairs rectangles)


getOverlapCounts :: [(Rectangle, Rectangle)] -> Map Point Integer
getOverlapCounts pairs = getOverlapCounts' Map.empty pairs


getOverlapCounts' :: Map Point Integer -> [(Rectangle, Rectangle)] -> Map Point Integer
getOverlapCounts' counts [] = counts
getOverlapCounts' counts (pair:pairs) =
  increment counts (getOverlappingPoints (fst pair) (snd pair))
  where
    increment counts [] = counts
    increment counts (p:ps) = increment (Map.insert p (1 + (fromMaybe 1 (counts !? p))) counts) ps


getOverlappingPoints :: Rectangle -> Rectangle -> [Point]
getOverlappingPoints r1 r2 = [(i, j) |
                               i <- [(fst xo)..((snd xo) - 1)],
                               j <- [(fst yo)..((snd yo) - 1)]]
  where
    xo = xOverlap r1 r2
    yo = yOverlap r1 r2


-- Hack: returning (n, n-1) tuple in case of no overlap yields empty list [n..(n-1)] of points in
-- interval.

xOverlap :: Rectangle -> Rectangle -> Interval
xOverlap r1 r2 = if overlap > 0 then (x2, x1 + dx1) else (x2, x2 - 1)
  where
    overlap = x1 + dx1 - x2
    (x1, _, dx1, _) = minimum [r1, r2]
    (x2, _, dx2, _) = maximum [r1, r2]


yOverlap :: Rectangle -> Rectangle -> Interval
yOverlap r1 r2 = if overlap > 0 then (y2, y1 + dy1) else (y2, y2 - 1)
  where
    overlap = y1 + dy1 - y2
    (_, y1, _, dy1) = if y1' < y2' then r1 else r2
    (_, y2, _, dy2) = if y1' < y2' then r2 else r1
    (_, y1', _, _) = r1
    (_, y2', _, _) = r2


unorderedPairs :: [a] -> [(a, a)]
unorderedPairs xs = [(xs !! i, xs !! j) | i <- [0..(n - 1)], j <- [0..(n - 1)], i < j]
  where
    n = length xs


main = do
  -- let rectangles = map parseLine $ lines "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2"
  -- print $ xOverlap (rectangles !! 0) (rectangles !! 1)
  -- print $ yOverlap (rectangles !! 0) (rectangles !! 1)
  -- print $ getOverlappingPoints (rectangles !! 0) (rectangles !! 1)
  -- print $ Map.toList $ getOverlapCounts (unorderedPairs rectangles)
  -- print $ part1 rectangles

  rectangles <- getInput
  -- print $ xOverlap (rectangles !! 0) (rectangles !! 1)
  -- print $ yOverlap (rectangles !! 0) (rectangles !! 1)
  -- print $ getOverlappingPoints (rectangles !! 0) (rectangles !! 1)
  print $ part1 rectangles
