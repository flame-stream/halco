module Calco.Utils.Data.Traversable where

import           Data.Map (Map, (!?))
import qualified Data.Map as Map

-- Counts occurences of each element in traversable.
countOccs :: (Traversable t, Ord k, Integral i) => t k -> Map k i
countOccs = foldr f Map.empty
  where
    f x m = case m !? x of
      Just i  -> Map.insert x (i + 1) m
      Nothing -> Map.insert x 1 m

maximumIntegral :: (Traversable t, Integral a) => t a -> a
maximumIntegral xs | null xs   = 0
                   | otherwise = maximum xs
