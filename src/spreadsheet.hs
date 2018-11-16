import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read (readMaybe)


-- A spreadsheet is a map from cell keys (e.g. a coordinate like "A3") to cell values
type Sheet = Map Key Value

-- A cell key is always a string
type Key = String

-- A cell value may be an unevaluated expression or a number
data Value = Unevaluated String | Evaluated Float
  deriving Show


sheet = Map.fromList
  [
    ("A1", Unevaluated "1"),
    ("A2", Unevaluated "2"),
    ("A3", Unevaluated "sum A1 A2"),
    ("A4", Unevaluated "5"),
    ("A5", Unevaluated "6"),
    ("A6", Unevaluated "product A4 A5"),
    ("A7", Unevaluated "sum A3 A6")
  ]


isNumeric :: String -> Bool
isNumeric string = case readMaybe string :: Maybe Float of
  Nothing -> False
  Just _ -> True


getFunction :: String -> ([Float] -> Float)
getFunction value = case fn of
  "sum" -> foldr (+) 0
  "product" -> foldr (*) 1
  where
    fn : refs = words value


getReferences :: String -> [String]
getReferences value = refs
  where
    fn : refs = words value


evaluateCell :: Key -> Sheet -> Sheet
evaluateCell key sheet = Map.insert key (Evaluated value) sheet
  where
    value = if isNumeric unevaluated
            then (read unevaluated :: Float)
            else function floats
    Just (Unevaluated unevaluated) = Map.lookup key sheet
    function = getFunction unevaluated
    floats = [getEvaluated ref (evaluateCell ref sheet) | ref <- getReferences unevaluated]
    getEvaluated string sheet = case Map.lookup string sheet of
      Just (Evaluated float) -> float


evaluate :: Sheet -> Sheet
evaluate sheet = foldr evaluateCell sheet (Map.keys sheet)


main = do
  print $ evaluate sheet
