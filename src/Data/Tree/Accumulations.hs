{-# LANGUAGE BangPatterns  #-}

module Data.Tree.Accumulations
  (Tree
  ,spanTree
  ,pop
  ,ins
  ,buildTree
  ,replicateATree
  ,fromIndList
  ,popElem
  ,singleton
  ,size)
  where

import           Control.Applicative
import           Control.Monad.State
import           Data.Bits
import           Data.List.Uncons

-- | A data type for efficiently performing selection without
-- replacement.
data Tree a
    = Tip
    | Bin {-# UNPACK #-} !Int
          !a
          !(Tree a)
          !(Tree a)
    deriving (Eq,Show)

instance Foldable Tree where
    foldr f = go
      where
        go b Tip = b
        go b (Bin _ x l r) = go (f x (go b r)) l


pop :: Int -> Tree a -> (a, Tree a)
pop !i t =
    case t of
        Tip -> errorWithoutStackTrace "pop: index out of range"
        Bin _ x l r ->
            case compare i sizeL of
                LT ->
                    case pop i l of
                        (y,l') -> (y, balanceR x l' r)
                GT ->
                    case pop (i - sizeL - 1) r of
                        (y,r') -> (y, balanceL x l r')
                EQ -> (x, glue l r)
            where
              sizeL = size l

glue :: Tree a -> Tree a -> Tree a
glue Tip r = r
glue l Tip = l
glue l@(Bin sl xl ll lr) r@(Bin sr xr rl rr)
  | sl > sr   = case maxView xl ll lr of (m, l') -> balanceR m l' r
  | otherwise = case minView xr rl rr of (m, r') -> balanceL m l r'

maxView :: t -> Tree t -> Tree t -> (t, Tree t)
maxView = go
  where
    go x l Tip = (x,l)
    go x l (Bin _ xr rl rr) =
      case go xr rl rr of
        (xm,r') -> (xm,balanceL x l r')

minView :: t -> Tree t -> Tree t -> (t, Tree t)
minView = go
  where
    go x Tip r = (x,r)
    go x (Bin _ xl ll lr) r =
      case go xl ll lr of
        (xm,l') -> (xm,balanceR x l' r)

size :: Tree a -> Int
size Tip = 0
size (Bin i _ _ _) = i

ins :: Int -> a -> Tree a -> Tree a
ins !_ x Tip = Bin 1 x Tip Tip
ins !i x (Bin _ y l r) = case compare i sizeL of
  LT -> case ins i x l of
    l' -> balanceL y l' r
  GT -> case ins (i-sizeL-1) x r of
    r' -> balanceR y l r'
  EQ -> balanceR x l (insertMin y r)
 where
   sizeL = size l

insertMin :: t -> Tree t -> Tree t
insertMin x t
  = case t of
      Tip -> Bin 1 x Tip Tip
      Bin _ y l r
          -> balanceL y (insertMin x l) r
-- |
--
-- prop> toList (buildTree xs) === xs
-- prop> buildTree [n..m] === spanTree n m
buildTree :: [a] -> Tree a
buildTree xs = evalState (replicateATree (length xs) uncons) xs

-- | Construct a tree to span a given inclusive range.
--
-- prop> toList (spanTree n m) === [n..m]
spanTree :: Int -> Int -> Tree Int
spanTree l u
  | l > u = Tip
  | otherwise = Bin (u - l + 1) m (spanTree l (m - 1)) (spanTree (m + 1) u)
  where
    m = shiftR (l + u) 1

replicateATree :: Applicative f => Int -> f a -> f (Tree a)
replicateATree sz xs = go sz
  where
    go 0 = pure Tip
    go n = liftA3 (\l x r -> Bin n x l r) (go ls) xs (go rs)
      where
        rs = shiftR n 1
        ls = n-rs-1

fromIndList :: [Int] -> Tree a -> [a]
fromIndList = evalState . traverse (state . pop)

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Data.Foldable (toList)

balanceL :: a -> Tree a -> Tree a -> Tree a
balanceL x l r =
    case r of
        Tip ->
            case l of
                Tip -> Bin 1 x Tip Tip
                Bin _ _ Tip Tip -> Bin 2 x l Tip
                Bin _ lx Tip (Bin _ lrx _ _) ->
                    Bin 3 lrx (Bin 1 lx Tip Tip) (Bin 1 x Tip Tip)
                Bin _ lx ll@Bin{} Tip -> Bin 3 lx ll (Bin 1 x Tip Tip)
                Bin ls lx ll@(Bin lls _ _ _) lr@(Bin lrs lrx lrl lrr)
                  | lrs < ratio * lls ->
                      Bin (1 + ls) lx ll (Bin (1 + lrs) x lr Tip)
                  | otherwise ->
                      Bin
                          (1 + ls)
                          lrx
                          (Bin (1 + lls + size lrl) lx ll lrl)
                          (Bin (1 + size lrr) x lrr Tip)
        Bin rs _ _ _ ->
            case l of
                Tip -> Bin (1 + rs) x Tip r
                Bin ls lx ll lr
                  | ls > delta * rs ->
                      case (ll, lr) of
                          (Bin lls _ _ _,Bin lrs lrx lrl lrr)
                            | lrs < ratio * lls ->
                                Bin
                                    (1 + ls + rs)
                                    lx
                                    ll
                                    (Bin (1 + rs + lrs) x lr r)
                            | otherwise ->
                                Bin
                                    (1 + ls + rs)
                                    lrx
                                    (Bin (1 + lls + size lrl) lx ll lrl)
                                    (Bin (1 + rs + size lrr) x lrr r)
                          (_,_) -> errorWithoutStackTrace "balanceL"
                  | otherwise -> Bin (1 + ls + rs) x l r
{-# NOINLINE balanceL #-}

balanceR :: a -> Tree a -> Tree a -> Tree a
balanceR x l r =
    case l of
        Tip ->
            case r of
                Tip -> Bin 1 x Tip Tip
                Bin _ _ Tip Tip -> Bin 2 x Tip r
                Bin _ rx Tip rr@Bin{} -> Bin 3 rx (Bin 1 x Tip Tip) rr
                Bin _ rx (Bin _ rlx _ _) Tip ->
                    Bin 3 rlx (Bin 1 x Tip Tip) (Bin 1 rx Tip Tip)
                Bin rs rx rl@(Bin rls rlx rll rlr) rr@(Bin rrs _ _ _)
                  | rls < ratio * rrs ->
                      Bin (1 + rs) rx (Bin (1 + rls) x Tip rl) rr
                  | otherwise ->
                      Bin
                          (1 + rs)
                          rlx
                          (Bin (1 + size rll) x Tip rll)
                          (Bin (1 + rrs + size rlr) rx rlr rr)
        Bin ls _ _ _ ->
            case r of
                Tip -> Bin (1 + ls) x l Tip
                Bin rs rx rl rr
                  | rs > delta * ls ->
                      case (rl, rr) of
                          (Bin rls rlx rll rlr,Bin rrs _ _ _)
                            | rls < ratio * rrs ->
                                Bin
                                    (1 + ls + rs)
                                    rx
                                    (Bin (1 + ls + rls) x l rl)
                                    rr
                            | otherwise ->
                                Bin
                                    (1 + ls + rs)
                                    rlx
                                    (Bin (1 + ls + size rll) x l rll)
                                    (Bin (1 + rrs + size rlr) rx rlr rr)
                          (_,_) -> error "balanceR"
                  | otherwise -> Bin (1 + ls + rs) x l r
{-# NOINLINE balanceR #-}

ratio :: Int
ratio = 2
delta :: Int
delta = 3

popElem :: Ord a => a -> Tree a -> (Int, Tree a)
popElem = go 0
  where
    go :: Ord a => Int -> a -> Tree a -> (Int, Tree a)
    go !_ !_ Tip  = errorWithoutStackTrace "popElem: index out of bounds"
    go idx x (Bin _ kx l r) = case compare x kx of
      LT -> case go idx x l of
        (i, l') -> (i, balanceR kx l' r)
      GT -> case go (idx + size l + 1) x r of
        (i, r') -> (i, balanceL kx l r')
      EQ -> (idx + size l, glue l r)

instance Monoid (Tree a) where
    mempty = Tip
    mappend Tip r   = r
    mappend l Tip   = l
    mappend l@(Bin sizeL x lx rx) r@(Bin sizeR y ly ry)
      | delta*sizeL < sizeR = balanceL y (mappend l ly) ry
      | delta*sizeR < sizeL = balanceR x lx (mappend rx r)
      | otherwise           = glue l r

singleton :: a -> Tree a
singleton x = Bin 1 x Tip Tip
