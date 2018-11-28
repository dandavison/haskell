module CollatzConjecture (collatz) where

import Data.List (elemIndex)


collatz :: Integer -> Maybe Integer
collatz n
  | n >= 1 = toInteger <$> elemIndex 1 (iterate collatzMap n)
  | otherwise = Nothing
  where
    collatzMap n
      | even n = n `div` 2
      | otherwise = 3 * n + 1
