module Data.Coerce.Operators where

import Data.Coerce

infixr 9 #.
(#.) :: Coercible c b => (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce
{-# INLINE (#.) #-}

infixl 8 .#
(.#) :: Coercible b a => (b -> c) -> (a -> b) -> a -> c
(.#) pbc _ = coerce pbc
{-# INLINE (.#) #-}
