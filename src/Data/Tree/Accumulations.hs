{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE LambdaCase       #-}

module Data.Tree.Accumulations
  (Tree
  ,spanTree
  ,pop
  ,buildTree
  ,replicateATree
  ,fromIndList)
  where

import           Data.Bits
import           Data.List.Uncons
import           Control.Monad.State
import           Control.Applicative

-- | A data type for efficiently performing selection without
-- replacement.
data Tree a
    = Leaf
    | Node {
             -- | The index left at this position.
             _index  :: !a
           ,
             -- | The size of the left child.
             _lsize  :: {-# UNPACK #-} !Int
           , _lchild :: !(Tree a)
           , _rchild :: !(Tree a)
           } deriving (Show,Eq)

instance Foldable Tree where
    foldr f = go
      where
        go b Leaf = b
        go b (Node x _ l r) = go (f x (go b r)) l

-- | Construct a tree to span a given inclusive range.
--
-- prop> toList (spanTree n m) === [n..m]
spanTree :: Int -> Int -> Tree Int
spanTree l u
  | l > u = Leaf
  | otherwise = Node m (m - l) (spanTree l (m - 1)) (spanTree (m + 1) u)
  where
    m = shiftR (l + u) 1

-- |
--
-- prop> toList (buildTree xs) === xs
-- prop> buildTree [n..m] === spanTree n m
buildTree :: [a] -> Tree a
buildTree xs = evalState (replicateATree (length xs) uncons) xs

-- | Build a tree by replicating some applicative action n times.
replicateATree :: Applicative f => Int -> f a -> f (Tree a)
replicateATree sz xs = go sz
  where
    go 0 = pure Leaf
    go n = liftA3 (\l x r -> Node x ls l r) (go ls) xs (go rs)
      where
        rs = shiftR n 1
        ls = n-rs-1

-- | Remove the nth item from the tree.
pop :: Int -> Tree a -> (a, Tree a)
pop _ Leaf = (undefined, Leaf)
pop i (Node j s l r) =
    case compare i s of
        LT ->
            case pop i l of
                (i',l') -> (i', Node j (s - 1) l' r)
        EQ -> (j, merge l r)
        GT ->
            case pop (i - s - 1) r of
                (i',r') -> (i', Node j s l r')

-- | Convert a tree to a list according to
-- some indices.
fromIndList :: [Int] -> Tree a -> [a]
fromIndList = evalState . traverse (state . pop)

-- | Merge two non-overlapping trees.
merge :: Tree a -> Tree a -> Tree a
merge Leaf Leaf = Leaf
merge l Leaf = l
merge Leaf r = r
merge (Node y ys yl yr) r = Node key s' l' r
  where
    (key,s',l') = maxView y ys yl yr

-- | Takes an unpacked node constructor, returns the largest value
-- as well as the rest of the tree and its size.
maxView :: a -> Int -> Tree a -> Tree a -> (a, Int, Tree a)
maxView y s l Leaf = (y, s, l)
maxView y s l (Node x xs xl xr) =
    case maxView x xs xl xr of
        (ny,s',nr) -> (ny, s + s' + 1, Node y s l nr)

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Data.Foldable (toList)
