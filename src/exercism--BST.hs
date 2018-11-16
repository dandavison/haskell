module BST
  ( BST
  , bstLeft
  , bstRight
  , bstValue
  , empty
  , fromList
  , insert
  , singleton
  , toList
  , printTree
  ) where

import Data.Maybe (fromMaybe)

data BST a = BST
  { bstValue :: Maybe a
  , bstLeft :: Maybe (BST a)
  , bstRight :: Maybe (BST a)
  } deriving (Eq, Show)

printTree :: Num a => Show a => BST a -> IO ()
printTree tree = do
  putStr "("
  (putStr . show) (fromMaybe 0 $ bstValue tree)
  case bstLeft tree of
    Nothing -> putStr ""
    Just leftTree -> printTree leftTree
  case bstRight tree of
    Nothing -> putStr ""
    Just rightTree -> printTree rightTree
  putStr ")"

empty :: BST a
empty = BST {bstValue = Nothing, bstLeft = Nothing, bstRight = Nothing}

fromList :: Ord a => [a] -> BST a
fromList xs = foldr insert empty $ reverse xs

insert :: Ord a => a -> BST a -> BST a
insert x tree =
  case bstValue tree of
    Nothing -> tree {bstValue = Just x}
    Just nodeValue ->
      if x <= nodeValue
        then insertLeft x tree
        else insertRight x tree

insertLeft :: Ord a => a -> BST a -> BST a
insertLeft x tree =
  case bstLeft tree of
    Nothing -> tree {bstLeft = Just $ singleton x}
    Just leftTree -> tree {bstLeft = Just $ insert x leftTree}

insertRight :: Ord a => a -> BST a -> BST a
insertRight x tree =
  case bstRight tree of
    Nothing -> tree {bstRight = Just $ singleton x}
    Just rightTree -> tree {bstRight = Just $ insert x rightTree}

singleton :: a -> BST a
singleton x = BST {bstValue = Just x, bstLeft = Nothing, bstRight = Nothing}

toList :: BST a -> [a]
toList tree = leftList ++ nodeList ++ rightList
  where
    leftList =
      case bstLeft tree of
        Nothing -> []
        Just leftTree -> toList leftTree
    rightList =
      case bstRight tree of
        Nothing -> []
        Just rightTree -> toList rightTree
    nodeList =
      case bstValue tree of
        Nothing -> []
        Just nodeValue -> [nodeValue]


-- A friend suggested
--
-- toList tree = takeBranch bstLeft ++ maybe [] pure (bstValue tree) ++ takeBranch bstRight
--   where
--     takeBranch f = concatMap toList $ f tree
-- or  takeBranch f = fromMaybe [] $ fmap toList $ f tree
-- or  takeBranch f = foldMap toList $ f tree
