module Calco.Utils where

import           Data.Map (Map, (!?))
import qualified Data.Map as Map

countOccs :: (Traversable t, Ord k, Integral i) => t k -> Map k i
countOccs = foldr f Map.empty
  where
    f x m = case m !? x of
      Just i  -> Map.insert x (i + 1) m
      Nothing -> Map.insert x 1 m

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct []       = []
cartesianProduct [xs]     = (: []) <$> xs
cartesianProduct (xs:xss) = [x : ys | ys <- cartesianProduct xss, x <- xs]

findKeys :: (Ord k, Eq a) => a -> Map k a -> [k]
findKeys x = map fst . filter ((== x) . snd) . Map.toList

maximumIntegral :: Integral a => [a] -> a
maximumIntegral [] = 0
maximumIntegral xs = maximum xs
