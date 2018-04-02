module Data.Permutations.Lexicographic where

-- | Produces permutations in lexicographic order.
--
-- >>> permutations "abc"
-- ["abc","acb","bac","bca","cab","cba"]
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = go id xs
  where
    go _ [] = []
    go k (y:ys) = map (y :) (permutations (k ys)) ++ go (k . (:) y) ys
