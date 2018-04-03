{-# LANGUAGE RankNTypes #-}

module Data.Permutations.Indexed.Vector.Unboxed where

import           Data.Vector.Unboxed       (Vector)
import qualified Data.Vector.Unboxed       as Vector

import           Control.Lens

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
permuted
    :: (Vector.Unbox a, Vector.Unbox b)
    => Permutation -> Iso (Vector a) (Vector b) (Vector a) (Vector b)
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
