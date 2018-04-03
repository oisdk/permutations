{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Tree.Accumulations
  (Tree
  ,spanTree
  ,pop
  ,buildTree
  ,replicateATree
  ,fromIndList)
  where

import           Control.Applicative
import           Control.Monad.State
import           Data.Bits
import           Data.List.Uncons

import           Data.Set.Internal   (Set (..), fromDistinctAscList, link,
                                      merge, size)

-- | A data type for efficiently performing selection without
-- replacement.
newtype Tree a = Tree
    (Set a)
    deriving (Show,Eq,Foldable)

pop :: Int -> Tree a -> (a, Tree a)
pop !i (Tree t) =
    case t of
        Tip -> errorWithoutStackTrace "pop: index out of range"
        Bin _ x l r ->
            case compare i sizeL of
                LT ->
                    case pop i (Tree l) of
                        (y,Tree l') -> (y, Tree (link x l' r))
                GT ->
                    case pop (i - sizeL - 1) (Tree r) of
                        (y,Tree r') -> (y, Tree (link x l r'))
                EQ -> (x, Tree (merge l r))
            where sizeL = size l
-- |
--
-- prop> toList (buildTree xs) === xs
-- prop> buildTree [n..m] === spanTree n m
buildTree :: [a] -> Tree a
buildTree = Tree . fromDistinctAscList

-- | Construct a tree to span a given inclusive range.
--
-- prop> toList (spanTree n m) === [n..m]
spanTree :: Int -> Int -> Tree Int
spanTree n m = buildTree [n..m]

replicateATree :: Applicative f => Int -> f a -> f (Tree a)
replicateATree n x = fmap buildTree (replicateM n x)


fromIndList :: [Int] -> Tree a -> [a]
fromIndList = evalState . traverse (state . pop)

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Data.Foldable (toList)
