import Data.List.Split


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


part1 :: [Rectangle] -> Integer
part1 rectangles = foldr (+) 0 (map overlapSize (unorderedPairs rectangles))


overlapSize :: (Rectangle, Rectangle) -> Integer
overlapSize (r1, r2) = (xOverlap r1 r2) * (yOverlap r1 r2)


xOverlap :: Rectangle -> Rectangle -> Integer
xOverlap r1 r2 = maximum [overlap, 0]
  where
    overlap = x1 + dx1 - x2
    (x1, _, dx1, _) = minimum [r1, r2]
    (x2, _, dx2, _) = maximum [r1, r2]


yOverlap :: Rectangle -> Rectangle -> Integer
yOverlap r1 r2 = maximum [overlap, 0]
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
  rectangles <- getInput
  print $ part1 rectangles
