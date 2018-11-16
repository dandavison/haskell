module Pangram (isPangram) where

import Data.Char (toLower)
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Hspec (hspec, describe, it, shouldBe)

-- The API called for in the exercise
isPangram :: String -> Bool
isPangram string = isPangram' string "abcdefghijklmnopqrstuvwxyz"

-- A more general function that allows the alphabet to be specified as a string
isPangram' :: String -> String -> Bool
isPangram' string alphabet = isPangram'' (toLower <$> string) (Set.fromList $ toLower <$> alphabet)

-- A private helper function that does the actual work
isPangram'' :: String -> Set Char -> Bool
isPangram'' string alphabet
  | alphabet == Set.empty = True
  | string == "" = False
  | otherwise = isPangram'' xs (Set.delete x alphabet)
  where
    (x:xs) = string

main :: IO ()
main = hspec $ do
  describe "isPangram'" $ do

    it "all empty" $ do
      isPangram' "" "" `shouldBe` True

    it "empty alphabet" $ do
      isPangram' "a" "" `shouldBe` True

    it "empty string" $ do
      isPangram' "" "a" `shouldBe` False

    it "a a" $ do
      isPangram' "a" "a" `shouldBe` True

    it "a b" $ do
      isPangram' "a" "b" `shouldBe` False

    it "ab b" $ do
      isPangram' "ab" "b" `shouldBe` True

    it "The quick brown fox jumps over the lazy dog" $ do
      isPangram'
        "the quick brown fox jumps over the lazy dog"
        "abcdefghijklmnopqrstuvwxyz" `shouldBe` True
