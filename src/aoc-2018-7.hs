import qualified Data.Heap as  Heap
import Data.Heap              (MinHeap)
import qualified Data.Map as   Map
import Data.Map               (Map, (!?))
import Data.Maybe             (fromMaybe)
import qualified Data.Set as   Set
import Data.Set               (Set)


type Node = Char
type Edge = (Node, Node)
type AdjacencyList = [(Node, [Node])]


makeGraph :: [Edge] -> AdjacencyList
makeGraph es = Map.toList $ makeGraph' Map.empty es

makeGraph' :: Map Node [Node] -> [Edge] -> Map Node [Node]
makeGraph' n2ns [] = n2ns
makeGraph' n2ns ((u, v):es) = makeGraph' n2ns'' es
  where
    n2ns' = Map.insert u ns' n2ns
    ns' = v:(fromMaybe [] (n2ns !? u))
    n2ns'' = Map.insert v (fromMaybe [] (n2ns' !? v)) n2ns'


getInput = do
  contents <- getContents
  return $ map parseLine (lines contents)


parseLine :: String -> Edge
parseLine line = (from, to)
  where
    from = (words line) !! 1 !! 0
    to = (words line) !! 7 !! 0


part1 = topologicalSort . makeGraph


topologicalSort :: AdjacencyList -> [Node]
topologicalSort n2ns = reverse $ topologicalSort' [] n2ns

topologicalSort' nodeSeq [] = nodeSeq
topologicalSort' nodeSeq n2ns = topologicalSort' (next:nodeSeq) n2ns'
  where
    descendents = mconcat $ snd <$> n2ns
    next = minimum [n | (n, ns) <- n2ns, n `notElem` descendents]
    n2ns' = [(n, ns) | (n, ns) <- n2ns, n /= next]



main = do
  points <- getInput
  print $ part1 points
