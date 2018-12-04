import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Map (Map, (!?), (!))

type Event = (Time, EventType)
data EventType = Guard GuardID | Wakes | Sleeps deriving (Eq, Ord, Show)
type GuardID = Integer
type Time = (Year, Month, Day, Hour, Minute)
type Year = Integer
type Month = Integer
type Day = Integer
type Hour = Integer
type Minute = Integer

-- The main data structure: for each guard, a map of minute:<number-of-times-asleep>
type Minute2Count = Map Minute Integer
type Guard2Minute2Count = Map GuardID Minute2Count


getInput = do
  contents <- getContents  -- readFile "/Users/dan/tmp/aoc-2018/input/4.txt"
  return $ map parseLine (lines contents)


parseLine :: String -> Event
parseLine line = ((year, month, day, hour, minute), event)
  where
    year = read (slice 1 4 line) :: Integer
    month = read (slice 6 7 line) :: Integer
    day = read (slice 9 10 line) :: Integer
    hour = read (slice 12 13 line) :: Integer
    minute = read (slice 15 16 line) :: Integer
    eventTypeString = drop 19 line
    guard = read (tail ((words eventTypeString) !! 1)) :: Integer
    event = case head (words eventTypeString) of
      "Guard" -> Guard guard
      "wakes" -> Wakes
      "falls" -> Sleeps


slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)


part1 :: [Event] -> Integer
part1 events = mostAsleepGuard * mostAsleepMinute
  where
    guard2minute2count = processEventStream (sort events)
    guard2totalMinutes = [(g, totalMinutes (Map.toList m2n)) | (g, m2n) <- Map.toList guard2minute2count]
    totalMinutes minute2asleepCount = foldr (+) 0 [n | (m, n) <- minute2asleepCount]
    (mostAsleepGuard, _) = argmax (\(g, n) -> n) guard2totalMinutes
    (mostAsleepMinute, _) = argmax (\(m, n) -> n) (Map.toList (guard2minute2count ! mostAsleepGuard))


part2 :: [Event] -> GuardID
part2 events = guard * minute
  where
    guard2minute2count = processEventStream (sort events)
    guard2maxCountMinute = [(g, maximum [(n, m) | (m, n) <- Map.toList m2n]) |
                             (g, m2n) <- Map.toList guard2minute2count]
    (guard, (count, minute)) = argmax (\(g, (n, m)) -> n) guard2maxCountMinute


argmax :: (Eq a, Ord b) => (a -> b) -> [a] -> a
argmax f xs = xmax
  where
    [xmax] = [x | x <- xs, f x == fmax]
    fmax = maximum (f <$> xs)


processEventStream :: [Event] -> Guard2Minute2Count
processEventStream events =
  processEventStream' Map.empty firstGuard firstSleepTime (drop 2 events)
  where
    (_, Guard firstGuard) = events !! 0   -- the first event must be a guard beginning their shift
    (firstSleepTime, Sleeps) = events !! 1  -- and (hopefully!) the second event is the guard sleeping
                                            -- (will fail if the first guard never slept)


processEventStream' :: Guard2Minute2Count -> GuardID -> Time -> [Event] -> Guard2Minute2Count
processEventStream' guard2minute2count _ _ [] = guard2minute2count
processEventStream' guard2minute2count guard sleptAt ((time, eventType):es) = case eventType of
  Guard guard' -> processEventStream' guard2minute2count guard' (-1, -11, -111, -1111, -11111) es
  Sleeps -> processEventStream' guard2minute2count guard time es
  Wakes -> processEventStream' guard2minute2count' guard (-2, -22, -222, -2222, -22222) es
  where
    guard2minute2count' = Map.insert guard minute2count' guard2minute2count
    minute2count = fromMaybe Map.empty (guard2minute2count !? guard)
    minute2count' = incrementAsleepMinutes sleptAtMinutes timeMinutes minute2count
    (_, _, _, _, sleptAtMinutes) = sleptAt
    (_, _, _, _, timeMinutes) = time


-- Add all minutes between sleep and wake time to guard's entry in guard2minute2count
incrementAsleepMinutes :: Minute -> Minute -> Minute2Count -> Minute2Count
incrementAsleepMinutes sleptAtMinute wokeAtMinute minute2count
  | sleptAtMinute < 0 = error $ "null sleptAtMinute: " ++ (show sleptAtMinute)
  | wokeAtMinute < 0 = error $ "null wokeAtMinute " ++ (show wokeAtMinute)
  | sleptAtMinute > wokeAtMinute = error "sleptAtMinute > wokeAtMinute"
  | sleptAtMinute == wokeAtMinute = minute2count
  | otherwise = incrementAsleepMinutes (1 + sleptAtMinute) wokeAtMinute minute2count'
  where
    minute2count' = Map.insert sleptAtMinute count' minute2count
    count' = 1 + (fromMaybe 0 (minute2count !? sleptAtMinute))


main = do
  events <- getInput
  -- putStr $ unlines (map show (sort events))
  print $ part1 events
  print $ part2 events
