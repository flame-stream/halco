module Halco.Utils.Data.Map where

import           Data.Map (Map, (!?))
import qualified Data.Map as Map

findKeys :: (Ord k, Eq a) => a -> Map k a -> [k]
findKeys x = map fst . filter ((== x) . snd) . Map.toList

insertOrApply :: Ord k => k -> v -> (v -> v) -> Map k v -> Map k v
insertOrApply k v f m = case m !? k of
  Just v' -> Map.insert k (f v') m
  Nothing -> Map.insert k v m
