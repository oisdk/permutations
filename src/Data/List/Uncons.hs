{-# LANGUAGE LambdaCase #-}

module Data.List.Uncons where

import           Prelude             hiding (fail)

import           Control.Applicative (Alternative(..))
import           Control.Monad.Fail  (MonadFail (..))
import           Control.Monad.State (StateT (..))

uncons :: Applicative m => StateT [a] m a
uncons = StateT (\case
        [] -> errorWithoutStackTrace "uncons: empty list"
        (x:xs) -> pure (x, xs))
{-# INLINE uncons #-}

unconsSafe :: Alternative m => StateT [a] m a
unconsSafe = StateT (\case
        [] -> empty
        (x:xs) -> pure (x, xs))
{-# INLINE unconsSafe #-}

unconsNote :: MonadFail m => StateT [a] m a
unconsNote = StateT (\case
        [] -> fail "uncons: empty list"
        (x:xs) -> pure (x, xs))
{-# INLINE unconsNote #-}
