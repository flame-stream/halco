module Halco.Utils.Function where

comp2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
comp2 f g x y = f (g x y)
