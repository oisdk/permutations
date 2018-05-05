{-# LANGUAGE RecordWildCards #-}

module Numeric.Baseless
  (add
  ,diff
  ,NumDict(..)
  ,numDict)
  where

import           Data.List.Para
import           Prelude        hiding ((+), (-))
import qualified Prelude

data NumDict a
    = NumDict
    { (+) :: a -> a -> a
    , (-) :: a -> a -> a
    , inc :: a -> a
    , one :: a
    , zer :: a
    }

{-# INLINE numDict #-}
numDict :: Num a => NumDict a
numDict = NumDict {..}
    where
      (+) = (Prelude.+)
      (-) = (Prelude.-)
      inc = (Prelude.+) 1
      one = 1
      zer = 0

{-# INLINE add #-}
-- |
-- prop> \(Positive x) (Positive y) -> fromBaseless (add numDict id 10 (toBaseless x) (toBaseless y)) === show (x + y)
add :: Ord a => NumDict a -> (a -> a) -> a -> [a] -> [a] -> [a]
add NumDict {..} f = go
  where
    go i (x:xs) (y:ys)
      | m >= i = (m - i) : goc (f i) xs ys
      | otherwise = m : go (f i) xs ys
      where
        m = x + y
    go _ xs [] = xs
    go _ [] ys = ys
    goc i (x:xs) (y:ys)
      | m >= i = (m - i) : goc (f i) xs ys
      | otherwise = m : go (f i) xs ys
      where
        m = inc (x + y)
    goc i xs [] = para go' go'' xs i
    goc i [] ys = para go' go'' ys i
    go' x xs a i
      | m >= i = (m - i) : a (f i)
      | otherwise = m : xs
      where
        m = inc x
    go'' n
        | n == one = [zer,one]
        | otherwise = [one]

{-# INLINE diff #-}
-- |
-- prop> \(Positive x) (Positive y) -> x > y ==> fromBaseless (diff numDict id 10 (toBaseless x) (toBaseless y)) === show (x - y)
diff :: Ord a => NumDict a -> (a -> a) -> a -> [a] -> [a] -> [a]
diff NumDict {..} f = go (0 :: Word)
  where
    go p i (x:xs) (y:ys) =
        case compare x y of
            GT -> pad p (x - y : go 0 (f i) xs ys)
            EQ -> go (succ p) (f i) xs ys
            LT -> pad p ((i + x) - y : goc 0 (f i) xs ys)
    go _ _ [] [] = []
    go p _ xs [] = pad p xs
    go _ _ [] (_:_) = error "numeric underflow"

    goc p i (x:xs) (y':ys) =
        case compare x y of
            GT -> pad p (x - y : go 0 (f i) xs ys)
            EQ -> go (succ p) (f i) xs ys
            LT -> pad p ((i + x) - y : goc 0 (f i) xs ys)
      where
        y = inc y'

    goc _ i xs [] = para go' (const []) xs i
    goc _ _ [] (_:_) = error "numeric underflow"

    go' x xs a i = case compare x one of
      LT -> (i - one) : a (f i)
      GT -> (x - one) : xs
      EQ -> case xs of
        [] -> []
        _  -> zer : xs

    pad 0 xs = xs
    pad n xs = zer : pad (pred n) xs

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Control.Monad
-- >>> :{
-- toBaseless :: Integer -> [Integer]
-- toBaseless = reverse . map (read . pure) . show
-- fromBaseless :: [Integer] -> String
-- fromBaseless = show <=< reverse
-- :}
