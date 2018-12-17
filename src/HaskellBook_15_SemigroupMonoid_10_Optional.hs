module HaskellBook_15_SemigroupMonoid_10_Optional where

import Data.Monoid  -- (Sum, getSum) -- why doesn't this work?
import Test.Hspec (hspec, describe, it, shouldBe)


-- 15.10 Write the Monoid instance for our Maybe type renamed to Optional.
data Optional a = Only a | Nada deriving (Eq, Show)


instance Semigroup a => Semigroup (Optional a) where

  (<>) (Only x) (Only y) = Only ((<>) x y)
  (<>) (Only x) Nada = Only x
  (<>) Nada (Only x) = Only x
  (<>) Nada Nada = Nada


instance Monoid a => Monoid (Optional a) where
  mempty = Nada


test = hspec $ do
  describe "test" $ do

    it "Semigroup instance for Optional" $ do
      Only (Sum 1) <> Only (Sum 1) `shouldBe` Only (Sum 2)
      Only (Product 4) <> Only (Product 2) `shouldBe` Only (Product 8)
      Only (Sum 1) <> Nada `shouldBe` Only (Sum 1)
      Only [1] <> Nada `shouldBe` Only [1]
      Nada <> Only (Sum 1) `shouldBe` Only (Sum 1)

    it "Monoid instance for Optional" $ do
      mconcat [Only (Sum 1), Only (Sum 1)] `shouldBe` Only (Sum 2)


-- main = do
--   test
--   -- print $ Nada -- why does this fail?
