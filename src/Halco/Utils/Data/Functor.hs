module Halco.Utils.Data.Functor where

infixl 4 <$$>
fmap2, (<$$>) :: (Functor f, Functor g) => (a -> b) -> g (f a) -> g (f b)
fmap2 = fmap . fmap
(<$$>) = fmap2
