{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Permutations.Indexed
  (Permutation(..)
  ,permuteList
  ,toFact
  ,fromFact
  ,factLen
  ,indices
  ,fromIndices
  ,indicesLength
  ,permuted
  ,permuteTrav
  ,permuteOf)
  where

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
import           Data.Functor.Compose

-- | This type represents the nth permutation of some sequence,
-- lexicographically ordered.
--
-- For instance, the string @"abc"@ has 6 permutations:
--
-- @
-- "abc"
-- "acb"
-- "bac"
-- "bca"
-- "cab"
-- "cba"
-- @
--
-- To produce any of the above permutations, we can run the
-- permutation type on @"abc"@:
--
-- >>> permuteList 2 "abc"
-- "bac"
--
-- Permutations can be composed, either with the monoid instance:
--
-- >>> Permutation 2 <> Permutation 5
-- 3
--
-- Or with '+':
--
-- >>> Permutation 2 + Permutation 5
-- 3
--
-- prop> (permuteList n . permuteList m) xs === permuteList (n <> m) xs
--
-- Permutations can also be inverted:
--
-- >>> (2 - 2) :: Permutation
-- 0
--
-- >>> permuteList (2 - 2) "abc"
-- "abc"
--
-- prop> (permuteList n . permuteList (negate n)) xs === xs
-- prop> permuteList (n - n) xs === xs
--
-- Permutations are cyclic for some input sizes. For instance, although
-- @"abc"@ only has 6 unique permutations, if we run the 7th:
--
-- It wraps around. To perform the "wrap-around" operation on the stored
-- value, use the function 'wrapAround'.
--
-- >>> wrapAround 3 6
-- 0
newtype Permutation = Permutation
    { ind :: Natural
    } deriving (Eq,Ord,Enum)

instance Show Permutation where
    showsPrec n (Permutation x) = showsPrec n x

instance Semigroup Permutation where
    x <> y = fromIndices (permuteList y ([0..xl-yl-1] ++ map (max 0 (xl-yl)+) (indices x)))
      where
        xl = factLen y
        yl = factLen x

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

-- | Converts a permutation from a Lehmer code.
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
-- prop> factLen n === length (indices n)
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

-- | Calculates which permutation a given sequence of indices is.
--
-- prop> (fromIndices . indices) n === n
fromIndices :: [Int] -> Permutation
fromIndices xs = fromFact (evalState (traverse (state . popElem) xs) tr)
  where
    ln = length xs
    tr = spanTree 0 (ln-1)

-- | Calculates the nth permutation for a sequence of
-- a particular length.
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

-- | Truncate a permutation to its smaller equivalent
-- on sequences of a specified length.
wrapAround :: Int -> Permutation -> Permutation
wrapAround ln p
  | factLen p > ln = Permutation (ind p `mod` product [1.. toEnum ln])
  | otherwise = p

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

type Permuted x = Compose ((,) (Tree x)) (State [x])

liftPermuted :: a -> Permuted a a
liftPermuted x = Compose (singleton x, uncons)

runPermuted :: Permutation -> Permuted x a -> a
runPermuted n' (Compose (tr,xs)) = evalState xs (toList ts ++ ys)
  where
    n = wrapAround s n'
    l = factLen n
    s = size tr
    (ys,ts) = runState (traverse (state . pop) (map ((s-l)+) (toFact n))) tr

permuteTrav :: Traversable t => Permutation -> t a -> t a
permuteTrav n = runPermuted n . traverse liftPermuted

permuteOf :: ((a -> Permuted a a) -> s -> Permuted a t) -> Permutation -> s -> t
permuteOf trav n = runPermuted n . trav liftPermuted

-- $setup
-- >>> import Test.QuickCheck
-- >>> :{
-- instance Arbitrary Natural where
--   arbitrary = fmap (fromInteger . getNonNegative) arbitrary
--   shrink = map (fromIntegral . getNonNegative) . shrink . NonNegative . toInteger
-- instance Arbitrary Permutation where
--   arbitrary = fmap Permutation arbitrary
--   shrink = map Permutation . shrink . ind
-- :}
