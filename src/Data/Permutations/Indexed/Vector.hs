{-# LANGUAGE RankNTypes #-}

module Data.Permutations.Indexed.Vector where

import           Data.Vector               (Vector)
import qualified Data.Vector               as Vector

import           Control.Lens

import           Data.Permutations.Indexed hiding (permutation)

import           Numeric.Natural

-- |
--
-- >>> Vector.fromList "abc" ^. permutation 0
-- "abc"
-- >>> Vector.fromList "abc" ^. permutation 1
-- "acb"
-- >>> Vector.fromList "abc" ^. permutation 5
-- "cba"
-- >>> Vector.fromList "abc" ^. permutation 6
-- "abc"
permutation :: Natural -> Iso (Vector a) (Vector b) (Vector a) (Vector b)
permutation n = iso perm unperm
  where
    perm xs =
        Vector.backpermute xs (Vector.fromList (permute n (Vector.length xs)))
    unperm xs =
        Vector.update_ xs (Vector.fromList (permute n (Vector.length xs))) xs

-- $setup
-- >>> import Test.QuickCheck
