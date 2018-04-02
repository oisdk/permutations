module Data.List.Unfoldl
  (unfoldl)
  where

import           GHC.Base (build)

unfoldl :: (b -> Maybe (a, b)) -> b -> [a]
unfoldl f = go []
  where
    go a = maybe a (uncurry (go . (: a))) . f
{-# NOINLINE [1] unfoldl #-}

unfoldlFB :: (b -> Maybe (a, b)) -> b -> (a -> c -> c) -> c -> c
unfoldlFB f b c n = go n b
  where
    go a = maybe a (uncurry (go . (`c` a))) . f
{-# INLINE [0] unfoldlFB #-}

{-# RULES
"unfoldl"     [~1] forall f b. unfoldl f b = build (unfoldlFB f b)
"unfoldlList" [1]  forall f b. unfoldlFB f b (:) [] = unfoldl f b #-}
