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

parseTree :: [Int] -> Node
parseTree [] = undefined
parseTree (nChildren:nMetadata:rest) = Node
  { header = (nChildren, nMetadata),
    children = parseTrees nChildren childData,
    metadata = slice ((length rest) - nMetadata + 1) (length rest) rest,
    trailingData = rest
  }
  where
    childData = slice 0 ((length rest) - nMetadata) rest


parseTrees :: Int -> [Int] -> [Node]
parseTrees n treeData = parseTrees' [] n treeData

parseTrees' :: [Node] -> Int -> [Int] -> [Node]
parseTrees' trees 0 _ = trees
parseTrees' trees n treeData = node:(parseTrees (n - 1) (trailingData node))
  where
    node = parseTree treeData

main = do
  let treeData = parseLine "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
  print $ treeData
  print $ parseTree treeData
