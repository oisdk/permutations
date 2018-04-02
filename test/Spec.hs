{-# LANGUAGE RankNTypes #-}

module Main (main) where

import           Test.QuickCheck                  hiding (vector)
import           Test.Tasty
import           Test.Tasty.QuickCheck            hiding (vector)

import           Control.Lens
import qualified Data.Set                         as Set
import           Numeric.Natural

import           Data.Permutations.Indexed        hiding (permutation)
import           Data.Permutations.Indexed.Vector

import           Data.Vector                      (Vector)
import           Data.Vector.Lens

instance Arbitrary Natural where
    arbitrary = fmap getNonNegative arbitrary
    shrink = map getNonNegative . shrink . NonNegative


perms :: Positive Int -> [[Int]]
perms (Positive n) = map ((`permute` n) . toEnum) [0 .. product [1..n] - 1]

isoIsId :: (Eq s, Show s) => Iso' s a -> s -> Property
isoIsId l xs = (l %~ id) xs === xs

-- (=?=) :: (Show a, Eq a) => a -> a -> Either String String
-- (=?=) xs ys = bool Left Right (xs == ys) (show xs ++ " =?= " ++ show ys)

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
    testProperty
        "Permutation isomorphism"
        (\(NonNegative n) ->
              isoIsId (vector . permutation n :: Iso' [Int] (Vector Int)))


main :: IO ()
main = defaultMain (testGroup "Tests" [prop_allLexPerms, prop_permIsId])
