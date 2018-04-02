module Data.Permutations.Indexed where

import           Data.List.Unfoldl
import           Data.Traversable
import           Data.Tree.Accumulations

import           Numeric.Natural

-- | Converts a number to its representation in the factorial number
-- system.
toFact :: Natural -> [Int]
toFact n' = unfoldl (uncurry go) (n', 1)
  where
    go 0 _ = Nothing
    go n m =
        case n `quotRem` m of
            (q,r) -> Just (fromEnum r, (q, m + 1))

-- | Calculates the length of the factorial representation of a number.
factLen :: Natural -> Int
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
    safeSub n m
        | n >= m = n - m
        | otherwise = 0

-- | Calculate the nth permutation.
--
-- >>> permutation 0
-- []
-- >>> permutation 1
-- [1,0]
permutation :: Natural -> [Int]
permutation n = snd (mapAccumL pop (spanTree 0 (prml - 1)) prms)
  where
    prms = toFact n
    prml = factLen n

-- | @'permute' n m@ calculates the @n@th permutation of length @m@.
--
-- >>> permute 0 5
-- [0,1,2,3,4]
--
-- >>> permute 3 5
-- [0,1,3,4,2]
permute :: Natural -> Int -> [Int]
permute _ 0 = []
permute n ln | factLen n > ln = permute (n - fact ln) ln
             | otherwise = [0 .. (ln - prml) - 1] ++ map ((ln - prml) +) (permutation n)
  where
    prml = factLen n
    fact m = product [1 .. toEnum m]
