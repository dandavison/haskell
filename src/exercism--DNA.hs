module DNA (toRNA) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

DNA2RNA :: Map Char Char
DNA2RNA = Map.fromList
  [
    'G': 'C',
    'C': 'G',
    'T': 'A'
    'A': 'U'
  ]


toRNA :: String -> Either Char String
toRNA (x:xs) = case Map.lookup x DNA2RNA of
  Just rnaNucleotide -> Right $ rnaNucleotide : toRNA xs
  Nothing -> Left x
