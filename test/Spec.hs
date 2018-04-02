module Main (main) where

import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.SmallCheck

import qualified Data.Set as Set

import Data.Permutations.Indexed

perms :: Positive Int -> [[Int]]
perms (Positive n) = map (`permute` n) [0 .. product [1..n] - 1]

prop_allLexPerms :: TestTree
prop_allLexPerms =
    testProperty
        "Correct Lex Perms"
        (\n ->
              let xs = perms n
                  ys = Set.fromList xs
              in Set.size ys == length xs && Set.toList ys == xs)

main :: IO ()
main = defaultMain (testGroup "Tests" [prop_allLexPerms])
