-- Write a Monoid instance for a Maybe type which doesn’t require a Monoid for the contents. Reuse
-- the Monoid law QuickCheck properties and use them to validate the instance.

-- import HaskellBook_15_SemigroupMonoid_10_Optional

-- newtype First' a =
--   First' { getFirst :: Optional a }
--   deriving (Eq, Show)

import Test.QuickCheck


data First' a = Just' a | Nothing' deriving (Eq, Show)


instance Semigroup (First' a) where

  (Just' x) <> _ = (Just' x)
  Nothing' <> (Just' y) = (Just' y)
  Nothing' <> Nothing' = Nothing'


instance Monoid (First' a) where
  mempty = Nothing'


instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    x <- arbitrary
    return (Just' x)


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc x y z = x <> (y <> z) == (x <> y) <> z

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId = First' String -> Bool


main = do
  quickCheck (semigroupAssoc :: FirstMappend)
