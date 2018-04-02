{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import           Test.QuickCheck                          hiding (vector)
import           Test.Tasty
import           Test.Tasty.QuickCheck                    hiding (vector)

import           Control.Lens
import qualified Data.Set                                 as Set
import           Numeric.Natural

import           Data.Permutations.Indexed                hiding (permutation)
import           Data.Permutations.Indexed.Vector
import qualified Data.Permutations.Indexed.Vector.Unboxed as Unboxed

import           Data.Vector                              (Vector)
import           Data.Vector.Lens
import qualified Data.Vector.Unboxed                      as Unboxed

instance Arbitrary Natural where
    arbitrary = fmap getNonNegative arbitrary
    shrink = map getNonNegative . shrink . NonNegative


perms :: Positive Int -> [[Int]]
perms (Positive n) = map ((`permute` n) . toEnum) [0 .. product [1..n] - 1]

isoIsId :: (Eq s, Show s) => Iso' s a -> s -> Property
isoIsId l xs = (l %~ id) xs === xs

prop_allLexPerms :: TestTree
prop_allLexPerms = localOption (QuickCheckMaxSize 5) $
    testProperty
        "Correct Lex Perms"
        (\n ->
              let xs = perms n
                  ys = Set.fromList xs
              in Set.size ys == length xs && Set.toList ys == xs)

prop_permIsId :: TestTree
prop_permIsId =
    testGroup
        "Permutation isomorphism"
        [ testProperty
              "Boxed"
              (\n ->
                    isoIsId (vector . permutation n :: Iso' [Int] (Vector Int)))
        , testProperty
              "Unboxed"
              (\n ->
                    isoIsId
                        (iso Unboxed.fromList Unboxed.toList .
                         Unboxed.permutation n :: Iso' [Int] (Unboxed.Vector Int)))]


main :: IO ()
main = defaultMain (testGroup "Tests" [prop_allLexPerms, prop_permIsId])
