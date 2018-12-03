import qualified Data.Set as Set
import Data.Set (Set)
import System.IO (getLine, isEOF)


-- Read list of integers from stdin
getValues :: [Integer] -> IO [Integer]
getValues values = do
  eof <- isEOF
  if eof
  then return values
  else do
    line <- getLine
    values' <- getValues (values ++ [parseLine line])
    return values'


getValues' :: IO [Integer]
getValues' = do
  contents <- getContents
  return $ map parseLine (lines contents)


parseLine :: String -> Integer
parseLine ('+':xs) = read xs :: Integer
parseLine xs = read xs :: Integer


firstRepeatedValue :: Ord a => [a] -> Set a -> a
firstRepeatedValue (x:xs) seen =
  if (Set.member x seen)
  then x
  else firstRepeatedValue xs (Set.union seen (Set.singleton x))


cumulativeSum :: Num a => a -> [a] -> [a]
cumulativeSum start (x:xs) = next : (cumulativeSum next xs)
  where
    next = start + x
cumulativeSum start [] = [start]


firstRepeatedCumulativeSum :: (Ord a, Num a) => [a] -> a
firstRepeatedCumulativeSum values =
  firstRepeatedValue (cumulativeSum 0 (cycle values)) (Set.empty)


main :: IO ()
main = do
  -- values <- getValues []
  values <- getValues'
  -- part 1
  print $ foldr (+) 0 values
  -- part 2
  print $ firstRepeatedCumulativeSum values
