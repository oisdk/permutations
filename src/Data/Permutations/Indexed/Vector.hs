{-# LANGUAGE RankNTypes #-}

module Data.Permutations.Indexed.Vector where

import           Data.Vector               (Vector)
import qualified Data.Vector               as Vector

import           Control.Lens              hiding (indices)

import           Data.Permutations.Indexed hiding (permuted)

-- |
--
-- >>> Vector.fromList "abc" ^. permuted 0
-- "abc"
-- >>> Vector.fromList "abc" ^. permuted 1
-- "acb"
-- >>> Vector.fromList "abc" ^. permuted 5
-- "cba"
-- >>> Vector.fromList "abc" ^. permuted 6
-- "abc"
permuted :: Permutation -> Iso (Vector a) (Vector b) (Vector a) (Vector b)
permuted n = iso perm unperm
  where
    perm xs =
        Vector.backpermute
            xs
            (Vector.fromList (indicesLength n (Vector.length xs)))
    unperm xs =
        Vector.update_
            xs
            (Vector.fromList (indicesLength n (Vector.length xs)))
            xs


-- $setup
-- >>> import Test.QuickCheck
