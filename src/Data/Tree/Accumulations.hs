module Data.Tree.Accumulations where

import Data.Bits

-- | A data type for efficiently performing selection without
-- replacement.
data Tree
    = Leaf
    | Node {
             -- | The index left at this position.
             _index :: {-# UNPACK #-} !Int
           ,
             -- | The size of the left child.
             _lsize :: {-# UNPACK #-} !Int
           , _lchild :: !Tree
           , _rchild :: !Tree}

-- | Construct a tree to span a given inclusive range.
spanTree :: Int -> Int -> Tree
spanTree l u
  | l > u = Leaf
  | otherwise = Node m (m - l) (spanTree l (m - 1)) (spanTree (m + 1) u)
  where
    m = shiftR (l + u) 1

pop :: Tree -> Int -> (Tree, Int)
pop Leaf i = (Leaf, i)
pop (Node j s l r) i =
    case compare i s of
        LT ->
            case pop l i of
                (l',i') -> (Node j (s - 1) l' r, i')
        EQ -> (merge l r, j)
        GT ->
            case pop r (i - s - 1) of
                (r',i') -> (Node j s l r', i')

merge :: Tree -> Tree -> Tree
merge Leaf Leaf = Leaf
merge l Leaf = l
merge Leaf r = r
merge (Node y ys yl yr) r = Node key s' l' r
  where
    (key,s',l') = maxView y ys yl yr

maxView :: Int -> Int -> Tree -> Tree -> (Int, Int, Tree)
maxView y s l Leaf = (y, s, l)
maxView y s l (Node x xs xl xr) =
    case maxView x xs xl xr of
        (ny,s',nr) -> (ny, s + s' + 1, Node y s l nr)
