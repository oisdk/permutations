module Data.Stream where

infixr 5 :<
data Stream a = a :< Stream a

foldrStream :: (a -> b -> b) -> Stream a -> b
foldrStream f = go where
  go (x :< xs) = f x (go xs)

unfoldrStream :: (b -> (a, b)) -> b -> Stream a
unfoldrStream f = go where
  go x = case f x of
    (y,ys) -> y :< go ys
