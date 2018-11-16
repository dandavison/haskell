-- Return True for year iff
-- on every year that is evenly divisible by 4
--   except every year that is evenly divisible by 100
--     unless the year is also evenly divisible by 400

-- Let divN mean: year is divisible by N.
-- Use &, |, and ! for conjunction, disjunction and negation respectively.
-- We want:
-- div4 & !(div100 & !div400) <==>
-- div4 & (!div100 | div400)

module LeapYear (isLeapYear) where

import Control.Exception (assert)
import Test.QuickCheck


isDivisibleBy :: Integer -> Integer -> Bool
isDivisibleBy n k = n `rem` k == 0


isLeapYearBooleanOperators :: Integer -> Bool
isLeapYearBooleanOperators year = year `isDivisibleBy` 4 && (not (year `isDivisibleBy` 100) || year `isDivisibleBy` 400)


data TruthTableRow = TruthTableRow { isDivisibleBy4 :: Bool
                                   , isDivisibleBy100 :: Bool
                                   , isDivisibleBy400 :: Bool
                                   }

isLeapYearPatterns :: Integer -> Bool
isLeapYearPatterns year = matchTruthTableRow TruthTableRow { isDivisibleBy4   = year `isDivisibleBy` 4
                                                           , isDivisibleBy100 = year `isDivisibleBy` 100
                                                           , isDivisibleBy400 = year `isDivisibleBy` 400}
  where
    matchTruthTableRow TruthTableRow {isDivisibleBy4=False} = False
    matchTruthTableRow TruthTableRow {isDivisibleBy100=False} = True
    matchTruthTableRow TruthTableRow {isDivisibleBy400=True} = True
    matchTruthTableRow _ = False


isLeapYearGuards :: Integer -> Bool
isLeapYearGuards year
  | not (year `isDivisibleBy` 4) = False
  | not (year `isDivisibleBy` 100) = True
  | otherwise = year `isDivisibleBy` 400


prop_isLeapYear :: Integer -> Property
prop_isLeapYear year =
  isLeapYearBooleanOperators year === isLeapYearGuards (year - 1)


isLeapYear :: Integer -> Bool
isLeapYear year =
  assert ((isLeapYearBooleanOperators year == isLeapYearPatterns year) &&
          (isLeapYearBooleanOperators year == isLeapYearGuards year)) isLeapYearGuards year
