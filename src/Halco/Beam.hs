{-# LANGUAGE GADTs #-}

module Halco.Beam where

import           Data.Map                 (Map)
import qualified Data.Map                 as Map

import           Halco.EGraph             (EOp1, EOp2, ESource)
import           Halco.Utils.Data.Functor (fmap2, (<$$>))
import           Halco.Utils.Data.Map     (insertOrApply)

-- Core Beam transform that maps function on source
pardo :: (e -> [e']) -> ESource e -> ESource e'
pardo = concatMap

-- Core Beam transform that groups values with the same keys
groupByKey :: Ord k => [(k, v)] -> [(k, [v])]
groupByKey = Map.toList . foldr f Map.empty
  where
    f (k, v) = insertOrApply k [v] (v :)

-- Beam transform that groups source elements by key
groupBy :: Ord k => (e -> k) -> ESource e -> [(k, [e])]
groupBy k = groupByKey . map (\e -> (k e, e))

-- Beam transform that groups by a some number of named properties
groupBy' :: (Ord k, Ord k') => Map k (e -> k') -> ESource e -> [(Map k k', [e])]
groupBy' ks = groupByKey . map (\e -> (fmap ($ e) ks, e))

-- Core Beam transform that applies relational join to two sources.
-- k1, k2 - names of the sources.
-- k - key for join.
coGroupByKey :: Ord k => (k1, [(k, v1)]) -> (k2, [(k, v2)])
             -> [(k, ( (k1, [v1])
                     , (k2, [v2]) ))]
coGroupByKey (k1, vs1) (k2, vs2) =
  [(k, (,) (k1, vs1') (k2, vs2'))
    | (k , vs1') <- groupByKey vs1
    , (k', vs2') <- groupByKey vs2
    , k == k']

-- Beam transform that aggregates values with the same key
combinePerKey :: Ord k => ([v] -> v') -> [(k, v)] -> [(k, v')]
combinePerKey f = combineValues f . groupByKey

-- Core Beam transform
combineValues :: Ord k => ([v] -> v') -> [(k, [v])] -> [(k, v')]
combineValues = fmap2

-- Core Beam transform
flatten :: [ESource e] -> ESource e
flatten = concat

-- Core Beam transform
partition :: Integral i => i -> ESource e -> [ESource e]
partition n = fmap2 snd . fmap snd
            . groupBy ((`mod` n) . fst)
            . zip [0..]
