module Calco.Utils where

import           Data.Map (Map, (!?))
import qualified Data.Map as Map

countOccs :: (Traversable t, Ord a, Integral i) => t a -> Map a i
countOccs = foldr f Map.empty
  where
    f x m = case m !? x of
      Just i  -> Map.insert x (i + 1) m
      Nothing -> Map.insert x 1 m