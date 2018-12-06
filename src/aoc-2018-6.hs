import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Map (Map, (!), (!?))
import qualified Data.Set as Set
import Data.Set (Set)


count :: Ord a => [a] -> Map a Integer
count ks = count' Map.empty ks

count' :: Ord a => Map a Integer -> [a] -> Map a Integer
count' k2n (k:ks) = count' k2n' ks
  where
    k2n' = Map.insert k n' k2n
    n' = 1 + fromMaybe 0 (k2n !? k)
count' k2n [] = k2n


groupBy :: Ord b => (a -> b) -> (a -> c) -> [a] -> Map b [c]
groupBy f g xs = groupBy' Map.empty f g xs

groupBy' :: Ord b => Map b [c] -> (a -> b) -> (a -> c) -> [a] -> Map b [c]
groupBy' k2xs f g (x:xs) = groupBy' k2xs' f g xs
  where
    k2xs' = Map.insert (f x) xs' k2xs
    xs' = (g x) : (fromMaybe [] (k2xs !? (f x)))
groupBy' k2xs _ _ [] = k2xs


hasNonEmptyIntersection :: Ord a => [a] -> [a] -> Bool
hasNonEmptyIntersection xs ys = length (Set.intersection (Set.fromList xs) (Set.fromList ys)) > 0


type Distance = Integer
type Point = (Integer, Integer)
type DistancePoint = (Distance, Point)
type Box = (Point, Point)


getInput = do
  contents <- getContents
  return $ map parseLine (lines contents)


parseLine :: String -> Point
parseLine line = (x, y)
  where
    [x, y] = read <$> words (filter (/= ',') line)


part1 = mostOwnedPoints


part2 ps = length [p | (p, dps) <- point2distances ps, sumDistances p dps < 10000]
  where
    sumDistances p dps = foldr (+) 0 [d | (d, p') <- dps, p' /= p]


mostOwnedPoints ps = maximum [length owned | (owner, owned) <- Map.toList p2ownedSolely, owner `notElem` ownersOfExternalPoints]
  where
    ((minx, miny), (maxx, maxy)) = boundingBox ps
    externalPoints = [(i, j) | (i, j) <- boundingGrid ps, i `elem` [minx, maxx] || j `elem` [miny, maxy]]
    p2ownedSolely = groupBy fst snd [(owner, owned) | (owned, (_, owner)) <- point2soleOwner ps]
    ownersOfExternalPoints = [owner | (owner, owned) <- Map.toList p2ownedSolely, owned `hasNonEmptyIntersection` externalPoints]


-- For each point in the bounding grid with a unique owning named point: the (distance, identity)
-- of that named point.
point2soleOwner :: [Point] -> [(Point, DistancePoint)]
point2soleOwner ps = [(p, o) | (p, [o]) <- filter ((== 1) . length . snd) (point2owners ps)]


-- For each point in the bounding grid: the (distance, identity) of the closest ("owning") named
-- point(s).
point2owners :: [Point] -> [(Point, [DistancePoint])]
point2owners ps = [(p, minimal dps) | (p, dps) <- point2distances ps]


-- For each point in the bounding grid: sorted distances to all named points
point2distances :: [Point] -> [(Point, [DistancePoint])]
point2distances ps = [(p, sort $ distance p <$> ps) | p <- boundingGrid ps]


distance :: Point -> Point -> DistancePoint
distance p1 p2 = (d, p2)
  where
    d = abs (x1 - x2) + abs (y1 - y2)
    (x1, y1) = p1
    (x2, y2) = p2


-- Retain tuples that have minimum value (in their first slot)
minimal :: [(Distance, Point)] -> [(Distance, Point)]
minimal dps = [x | x <- dps, (fst x) == fst (minimum dps)]


boundingGrid :: [Point] -> [Point]
boundingGrid ps = [(x, y) | x <- [x1..x2], y <- [y1..y2]]
  where
    ((x1, y1), (x2, y2)) = boundingBox ps


boundingBox :: [Point] -> Box
boundingBox ps = ((minimum xs, minimum ys),
                  (maximum xs, maximum ys))
  where
    xs = fst <$> ps
    ys = snd <$> ps


main = do
  points <- getInput
  -- putStr $ unlines (show <$> mostOwnedPoints points)
  print $ part1 points
  print $ part2 points
