import Data.Monoid
import Test.Hspec (hspec, describe, it, shouldBe)


data Booly = False' | True' deriving (Eq, Show)


-- Monoid with OR semantics

instance Semigroup Booly where
  x <> y = if (x == True') || (y == True') then True' else False'


instance Monoid Booly where
  mempty = False'


test = hspec $ do
  describe "monoid with `or` semantics" $ do
    it "<>" $ do
      True' <> True' `shouldBe` True'
      True' <> False' `shouldBe` True'
      False' <> True' `shouldBe` True'
      False' <> False' `shouldBe` False'

    it "mconcat 2" $ do
      mconcat [True', True'] `shouldBe` True'
      mconcat [True' `shouldBe` True']
      mconcat [False', True'] `shouldBe` True'
      mconcat [False', False'] `shouldBe` False'

    it "mconcat 3" $ do
      mconcat [True', True', True'] `shouldBe` True'
      mconcat [True', True', False'] `shouldBe` True'
      mconcat [False', True', True'] `shouldBe` True'
      mconcat [False', True', False'] `shouldBe` True'


main = do
  test

