{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import           Test.QuickCheck                          hiding (vector)
import           Test.Tasty
import           Test.Tasty.QuickCheck                    hiding (vector)

import           Control.Lens                             hiding (indices)
import qualified Data.Set                                 as Set
import           Numeric.Natural

import           Data.Permutations.Indexed
import qualified Data.Permutations.Indexed.Vector         as Vector
import qualified Data.Permutations.Indexed.Vector.Unboxed as Unboxed

import           Data.Vector                              (Vector)
import           Data.Vector.Lens
import qualified Data.Vector.Unboxed                      as Unboxed

perms :: Positive Int -> [[Int]]
perms (Positive n) =
    map (flip indicesLength (fromEnum n) . Permutation . toEnum) [0 .. product [1 .. n] - 1]

isoIsId
    :: (Eq s, Show s)
    => Iso' s a -> s -> Property
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
                    isoIsId (vector . Vector.permuted n :: Iso' [Int] (Vector Int)))
        , testProperty
              "Unboxed"
              (\n ->
                    isoIsId
                        (iso Unboxed.fromList Unboxed.toList .
                         Unboxed.permuted n :: Iso' [Int] (Unboxed.Vector Int)))
        , testProperty
              "List"
              (\n ->
                    isoIsId (permuted n :: Iso' [Int] [Int]))]

instance Arbitrary Natural where
    arbitrary = fmap (fromInteger . getNonNegative) arbitrary
    shrink =
        map (fromIntegral . getNonNegative) . shrink . NonNegative . toInteger

instance Arbitrary Permutation where
    arbitrary = fmap Permutation arbitrary
    shrink = map Permutation . shrink . ind

main :: IO ()
main =
    defaultMain
        (testGroup
             "Tests"
             [ prop_allLexPerms
             , prop_permIsId
             , testProperty
                   "Inversion"
                   (\n xs ->
                         (permuteList n . permuteList (negate n)) xs ===
                         (xs :: String))
             , testProperty
                   "Negation"
                   (\n m xs ->
                         (permuteList n . permuteList (negate m)) xs ===
                         permuteList (n - m) (xs :: String))
             , testProperty
                   "Action"
                   (\n m xs ->
                         (permuteList n . permuteList m) xs ===
                         permuteList (n + m) (xs :: String))
             , testProperty
                   "Zero"
                   (\n ->
                         (n - n) === (0 :: Permutation))])
