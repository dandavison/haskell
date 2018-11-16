import qualified Data.Map as Map
import Data.Map (Map)

type Sheet = Map Key Value

data Key = Key String
  deriving (Show, Eq, Ord)

data Value = Unevaluated String | Evaluated Float
  deriving Show

sheet :: Sheet
sheet = Map.fromList [(Key "key", Unevaluated "val")]


main = do
  print $ Map.toList sheet
