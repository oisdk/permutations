module Data.List.Uncons where

import Control.Monad.State
import Control.Applicative

uncons :: Applicative m => StateT [a] m a
uncons = StateT go where
  go [] = errorWithoutStackTrace "uncons: empty list"
  go (x:xs) = pure (x,xs)
  {-# INLINE go #-}
{-# INLINE uncons #-}

unconsSafe :: Alternative m => StateT [a] m a
unconsSafe = StateT go where
  go [] = empty
  go (x:xs) = pure (x,xs)
  {-# INLINE go #-}
{-# INLINE unconsSafe #-}
