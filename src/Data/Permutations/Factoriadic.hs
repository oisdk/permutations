{-# LANGUAGE BangPatterns #-}

module Data.Permutations.Factoriadic where

import           Data.Coerce
import           Data.Coerce.Operators
import           Data.List.Para
import           Numeric.Natural
import           Data.Semigroup

-- | The Factoriadic representation of a number.
newtype Factoriadic = Factoriadic
    { runFactoriadic :: [Natural]
    } deriving Eq


-- |
-- prop> compare x y === compare (toInteger x) (toInteger (y :: Factoriadic))
instance Ord Factoriadic where
    compare = coerce (go EQ)
      where
        go :: Ordering -> [Natural] -> [Natural] -> Ordering
        go !a [] [] = a
        go !_ (_:_) [] = GT
        go !_ [] (_:_) = LT
        go !a (x:xs) (y:ys) = go (compare x y <> a) xs ys

-- |
--
-- prop> \ (NonNegative n) (NonNegative m) -> toInteger (fromInteger n + (fromInteger m :: Factoriadic)) === n + m
-- prop> \ (NonNegative n) (NonNegative m) -> m <= n ==> toInteger (fromInteger n - (fromInteger m :: Factoriadic)) === fromInteger (n - m)
-- prop> \ (NonNegative n) (NonNegative m) -> toInteger (fromInteger n * (fromInteger m :: Factoriadic)) === fromInteger (n * m)
instance Num Factoriadic where
    (+) = coerce (go 1)
      where
        go :: Natural -> [Natural] -> [Natural] -> [Natural]
        go i (x:xs) (y:ys)
          | m >= i = (m - i) : goc (i + 1) xs ys
          | otherwise = m : go (i + 1) xs ys
          where
            m = x + y
        go _ xs [] = xs
        go _ [] ys = ys
        goc i (x:xs) (y:ys)
          | m >= i = (m - i) : goc (i + 1) xs ys
          | otherwise = m : go (i + 1) xs ys
          where
            m = succ (x + y)
        goc i xs [] = para go' go'' xs i
        goc i [] ys = para go' go'' ys i
        go' x xs a i
          | m >= i = (m - i) : a (i + 1)
          | otherwise = m : xs
          where
            m = succ x
        go'' 1 = [0, 1]
        go'' _ = [1]
    {-# INLINE (+) #-}
    xs * ys = fromInteger (toInteger xs * toInteger ys)
    abs = id
    fromInteger = Factoriadic #. go 1
      where
        go _ 0 = []
        go i n =
            case divMod n i of
                (q,r) -> fromInteger r : go (i + 1) q
    signum (Factoriadic []) = Factoriadic []
    signum _ = Factoriadic [0, 1]
    (-) = coerce (go 0 1)
      where
        go :: Integer -> Integer -> [Natural] -> [Natural] -> [Natural]
        go c i (x:xs) (y:ys) =
            case divMod (toInteger x + c - toInteger y) i of
                (q,r) -> fromInteger r : go q (i + 1) xs ys
        go c i xs [] = para go' go'' xs c i
        go c i [] ys = para go' go'' ys c i
        go' x xs _ 0 _ = x : xs
        go' x _ xs c i =
            case divMod (toInteger x + c) i of
                (q,r) -> fromInteger r : xs q (i + 1)
        go'' 0 _ = []
        go'' c i =
            case divMod c i of
                (q,r) -> fromInteger r : go'' q (i + 1)

instance Real Factoriadic where
    toRational = toRational . toInteger

-- |
-- prop> succ x === (x :: Factoriadic) + 1
instance Enum Factoriadic where
    toEnum = Factoriadic #. go 1
      where
        go _ 0 = []
        go i n =
            case divMod n i of
                (q,r) -> toEnum r : go (i + 1) q
    fromEnum (Factoriadic xs) =
        foldr (\e a b -> fromEnum e + b * a (b + 1)) (const 0) xs 1
    succ (Factoriadic xs') = Factoriadic (para go go' xs' 1) where
      go :: Natural -> [Natural] -> (Natural -> [Natural]) -> Natural -> [Natural]
      go x xs ys i
        | y == i = 0 : ys (i+1)
        | otherwise = y : xs
        where y = succ x
      go' 1 = [0,1]
      go' _ = [1]
    pred x = x - 1

instance Integral Factoriadic where
    toInteger (Factoriadic xs) =
        foldr (\e a b -> toInteger e + b * a (b + 1)) (const 0) xs 1
    quotRem n m =
        case quotRem (toInteger n) (toInteger m) of
            (q,r) -> (fromInteger q, fromInteger r)

instance Show Factoriadic where
    showsPrec n = showsPrec n . toInteger

-- $setup
-- >>> import Test.QuickCheck
-- >>> :{
-- instance Arbitrary Factoriadic where
--    arbitrary = fmap (fromInteger .# getNonNegative) arbitrary
-- :}
