module Calco.Utils where

import           Data.Map (Map, (!?))
import qualified Data.Map as Map
import           ListT

-- Counts occurences of each element in traversable.
countOccs :: (Traversable t, Ord k, Integral i) => t k -> Map k i
countOccs = foldr f Map.empty
  where
    f x m = case m !? x of
      Just i  -> Map.insert x (i + 1) m
      Nothing -> Map.insert x 1 m

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct []       = []
cartesianProduct [xs]     = (: []) <$> xs
cartesianProduct (xs:xss) = (:) <$> xs <*> cartesianProduct xss

findKeys :: (Ord k, Eq a) => a -> Map k a -> [k]
findKeys x = map fst . filter ((== x) . snd) . Map.toList

maximumIntegral :: Integral a => [a] -> a
maximumIntegral [] = 0
maximumIntegral xs = maximum xs

nilLT :: Monad m => ListT m a
nilLT = ListT $ pure Nothing

infixl 4 <$$>
fmap2, (<$$>) :: (Functor f, Functor g) => (a -> b) -> g (f a) -> g (f b)
fmap2 = fmap . fmap
(<$$>) = fmap2
