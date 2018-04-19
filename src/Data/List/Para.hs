module Data.List.Para where

para :: (a -> [a] -> b -> b) -> b -> [a] -> b
para f b = go
  where
    go [] = b
    go (x:xs) = f x xs (go xs)
{-# INLINE para #-}
