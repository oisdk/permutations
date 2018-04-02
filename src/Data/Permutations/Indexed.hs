module Data.Permutations.Indexed where

import           Data.List.Unfoldl
import           Data.Traversable
import           Data.Tree.Accumulations

-- | Converts a number to its representation in the factorial number
-- system.
toFact :: Int -> [Int]
toFact n' = unfoldl (uncurry go) (n', 1)
  where
    go 0 _ = Nothing
    go n m =
        case n `quotRem` m of
            (q,r) -> Just (r, (q, m + 1))

-- | Calculates the length of the factorial representation of a number.
factLen :: Int -> Int
factLen =
    length .
    takeWhile (> 0) .
    flip
        (scanl (-))
        (0 :
         snd
             (mapAccumL
                  (\f e ->
                        (f * e, f * e * e))
                  1
                  [1 ..]))

-- | Calculate the nth permutation.
permutation :: Int -> [Int]
permutation n = snd (mapAccumL pop (spanTree 0 (prml - 1)) prms)
  where
    prms = toFact n
    prml = factLen n

-- | @'permute' n m@ calculates the @n@th permutation of length @m@.
--
-- >>> 1
-- 2
permute :: Int -> Int -> [Int]
permute n ln = [0 .. ln - prml - 1] ++ map ((ln - prml) +) (permutation n)
  where
    prml = factLen n
