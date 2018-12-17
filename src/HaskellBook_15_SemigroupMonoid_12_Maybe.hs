-- Write a Monoid instance for a Maybe type which doesn’t require a Monoid for the contents. Reuse
-- the Monoid law QuickCheck properties and use them to validate the instance.
import HaskellBook_15_SemigroupMonoid_10_Optional

-- newtype First' a = Only a | Nada


-- instance Monoid First' where
--   mempty = Nada

main = do
  print $ Only "hello"
