import Data.List
import Data.List.NonEmpty (NonEmpty)


slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)


getInput = do
  input <- getContents
  return $ parseLine input


parseLine :: String -> [Int]
parseLine line = read <$> words line


type NChildren = Int
type NMetadata = Int
type Metadata = Int

data Node = Node
  { header :: (NChildren, NMetadata),
    children :: [Node],
    metadata :: [Metadata],
    trailingData :: [Int]
  }
  deriving Show

parseNode :: [Int] -> Node
parseNode [] = undefined
parseNode (nChildren:nMetadata:rest) = Node
  { header = (nChildren, nMetadata),
    children = parseNodes nChildren childData,
    metadata = slice ((length rest) - nMetadata + 1) (length rest) rest,
    trailingData = rest
  }
  where
    childData = slice 0 ((length rest) - nMetadata) rest


parseNodes :: Int -> [Int] -> [Node]
parseNodes n nodeData = parseNodes' [] n nodeData

parseNodes' :: [Node] -> Int -> [Int] -> [Node]
parseNodes' nodes n nodeData
  | n == 0 = nodes
  | (trailingData node) == [] = node:nodes
  | otherwise = node:(parseNodes' nodes (n - 1) (trailingData node))
  where
    node = parseNode nodeData

main = do
  let nodeData = parseLine "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
  print $ nodeData
  print $ parseNodes 1 nodeData
