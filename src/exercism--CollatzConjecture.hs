module CollatzConjecture (collatz) where

import Data.List (elemIndex)


collatz :: Integer -> Maybe Integer
collatz n
  | n >= 1 = toInteger <$> elemIndex 1 (collatzSequence n)
  | otherwise = Nothing


collatzSequence :: Integer -> [Integer]
collatzSequence n = n : (collatzSequence next)
  where
    next
      | even n = n `div` 2
      | otherwise = 3 * n + 1
