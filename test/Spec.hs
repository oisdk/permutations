module Main (main) where

import Test.SmallCheck
import Test.Tasty
import Test.Tasty.SmallCheck

prop_noTests :: TestTree
prop_noTests = testProperty "Any Tests" (forAll False)

main :: IO ()
main = defaultMain (testGroup "Tests" [prop_noTests])
