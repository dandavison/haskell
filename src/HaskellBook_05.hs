kessel :: (Ord a, Num b) => a -> b -> a
kessel = undefined

f :: Integer
f = kessel (1 :: Integer) 2


main = do
  putStrLn "Done"

-- :t myConcat
-- [Char] -> [Char] .

-- :t myMult
-- Fractional a => a -> a .


-- :t myTake
-- Int -> [Char] .

-- :t myCom
-- Int -> Bool .

-- :t myAlph
-- Char -> Bool .


-- {-# LANGUAGE NoMonomorphismRestriction #-}

-- module DetermineTheType where

-- example = 1


-- x = 5
-- y = x + 5
-- z y = y * 10

-- -- :t z
-- -- Num a => a -> a

-- w = y / 5

-- -- f = 4 / y
