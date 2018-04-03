{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Data.Permutations.Indexed where

import           Data.List.Uncons
import           Data.List.Unfoldl
import           Data.Traversable
import           Data.Tree.Accumulations

import           Control.Applicative
import           Control.Lens            (Iso, iso)
import           Control.Monad.State
import           Data.Foldable           (toList)
import           Data.List               (sortBy)
import           Data.Ord
import           Numeric.Natural
import           Data.Semigroup

newtype Permutation = Permutation
    { ind :: Natural
    } deriving (Eq,Ord)

instance Show Permutation where
    showsPrec n (Permutation x) = showsPrec n x

-- |
-- prop> (permuteList n . permuteList m) xs === permuteList (n <> m) xs
instance Semigroup Permutation where
    x <> y = fromFact (toFact x ++ toFact y)

instance Monoid Permutation where
    mempty = Permutation 0
    mappend = (<>)

instance Num Permutation where
    fromInteger = Permutation . fromInteger
    (+) = (<>)
    negate = inv
    (*) = error "Multiplication not defined for permutations"
    abs = error "abs not defined for permutations"
    signum = error "signum not defined for permutations"

-- | Converts a permutation to its Lehmer code.
toFact :: Permutation -> [Int]
toFact (Permutation n') = unfoldl (uncurry go) (n', 1)
  where
    go 0 _ = Nothing
    go n m =
        case n `quotRem` m of
            (q,r) -> Just (fromEnum r, (q, m + 1))

-- |
--
-- prop> (fromFact . toFact) n === n
fromFact :: [Int] -> Permutation
fromFact xs = Permutation (foldl f b xs 1)
  where
    b _ = 0
    f a e n = toEnum e + n * a (n+1)

-- | Calculates the length of the factorial representation of a number.
--
-- prop> factLen n === length (toFact n)
factLen :: Permutation -> Int
factLen =
    length .
    takeWhile (> 0) .
    flip
        (scanl safeSub)
        (0 :
         snd
             (mapAccumL
                  (\f e ->
                        (f * e, f * e * e))
                  1
                  [1 ..]))
  where
    safeSub (Permutation n) m
        | n >= m = Permutation (n - m)
        | otherwise = 0

-- | Calculate the nth permutation.
--
-- >>> indices 0
-- []
-- >>> indices 1
-- [1,0]
indices :: Permutation -> [Int]
indices n = evalState (traverse (state . pop) prms) (spanTree 0 (prml - 1))
  where
    prms = toFact n
    prml = factLen n

-- |
-- prop> (fromIndices . indices) n === n
fromIndices :: [Int] -> Permutation
fromIndices xs = fromFact (evalState (traverse (state . popElem) xs) tr)
  where
    ln = length xs
    tr = spanTree 0 (ln-1)

indicesLength :: Permutation -> Int -> [Int]
indicesLength p' n =
    [0 .. d - 1] ++
    evalState
        (traverse (state . pop) (toFact p))
        (spanTree d (d + l - 1))
  where
    p = wrapAround n p'
    l = factLen p
    d = n - l


-- |
-- prop> (permuteList (inv n) . permuteList n) xs === xs
inv :: Permutation -> Permutation
inv = fromIndices . map fst . sortBy (comparing snd) . zip [0 ..] . indices

-- | Return the nth permutation of a list.
--
-- >>> permuteList 0 "abc"
-- "abc"
-- >>> permuteList 1 "abc"
-- "acb"
permuteList :: Permutation -> [a] -> [a]
permuteList n xs = evalState (permuteA (length xs) n uncons) xs

wrapAround :: Int -> Permutation -> Permutation
wrapAround ln = go . ind
  where
    go n
      | factLen (Permutation n) > ln = go (n - fln)
      | otherwise = Permutation n
    fln = product [1 .. toEnum ln]

-- | Invertly permute
--
-- prop> (invPermuteList n . permuteList n) xs == (xs :: String)
--
-- >>> (invPermuteList 3 . permuteList 3) "abc"
-- "abc"
-- >>> (invPermuteList 1 . permuteList 1) "abc"
-- "abc"
-- >>> (invPermuteList 2 . permuteList 2) "abc"
-- "abc"
-- >>> (invPermuteList 4 . permuteList 4) "abc"
-- "abc"
invPermuteList :: Permutation -> [a] -> [a]
invPermuteList n xs = evalState (invPermuteA (length xs) n uncons) xs

-- | Execute an applicative action n times, collecting the results in
-- order of the mth permutation.
permuteA :: Applicative f => Int -> Permutation -> f a -> f [a]
permuteA ln n' x =
    liftA2
        (\xs ys ->
              xs ++ fromIndList (toFact n) ys)
        (replicateM (ln - fln) x)
        (replicateATree fln x)
  where
    fln = factLen n
    n = wrapAround ln n'

invPermuteA :: Applicative f => Int -> Permutation -> f a -> f [a]
invPermuteA ln n' x =
    liftA2
        (\xs ys ->
              xs ++ f ys)
        (replicateM (ln - fln) x)
        (replicateM fln x)
  where
    n = wrapAround ln n'
    fln = factLen n
    f = toList . foldr (uncurry ins) (buildTree []) . zip (toFact n)


permuted :: Permutation -> Iso [a] [b] [a] [b]
permuted n = iso (permuteList n) (invPermuteList n)

-- $setup
-- >>> import Test.QuickCheck
-- >>> :{
-- instance Arbitrary Natural where
--   arbitrary = fmap getNonNegative arbitrary
--   shrink = map (fromIntegral . getNonNegative) . shrink . NonNegative . toInteger
-- instance Arbitrary Permutation where
--   arbitrary = fmap Permutation arbitrary
--   shrink = map Permutation . shrink . ind
-- :}
