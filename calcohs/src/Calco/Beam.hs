{-# LANGUAGE GADTs #-}

module Calco.Beam where

import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Tuple.Extra         (uncurry3)

import           Calco.Defs               (Attr)
import           Calco.Utils.Data.Functor (fmap2, (<$$>))
import           Calco.Utils.Data.Map     (insertOrApply)

type Stream e = [e]

-- Core Beam transform that maps function on stream.
pardo :: (e -> [e']) -> Stream e -> Stream e'
pardo = concatMap

-- Core Beam transform that groups values with the same keys.
groupByKey :: Ord k => [(k, v)] -> [(k, [v])]
groupByKey = Map.toList . foldr f Map.empty
  where
    f (k, v) = insertOrApply k [v] (v :)

-- Beam transform that groups stream elements by key.
groupBy :: Ord k => (e -> k) -> Stream e -> [(k, [e])]
groupBy k = groupByKey . map (\e -> (k e, e))

-- Beam transform that groups by a some number of named properties.
groupBy' :: (Ord k, Ord k') => Map k (e -> k') -> Stream e -> [(Map k k', [e])]
groupBy' ks = groupByKey . map (\e -> (fmap ($ e) ks, e))

-- Core Beam transform that applies relational join to two streams.
-- k1, k2 - names of the streams.
-- k - key for join.
coGroupByKey :: Ord k => (k1, [(k, v1)]) -> (k2, [(k, v2)])
             -> [(k, ( (k1, [v1])
                     , (k2, [v2]) ))]
coGroupByKey (k1, vs1) (k2, vs2) =
  map from $ groupBy fst
    [(k, (v1, v2))
      | (k , v1) <- vs1
      , (k', v2) <- vs2
      , k == k']
  where
    from (k, es) = (k, ( (k1, fst . snd <$> es)
                       , (k2, snd . snd <$> es) ))

-- Beam transform that aggregetes values with the same key.
combinePerKey :: Ord k => ([v] -> v') -> [(k, v)] -> [(k, v')]
combinePerKey f = combineValues f . groupByKey

-- Core Beam transform.
combineValues :: Ord k => ([v] -> v') -> [(k, [v])] -> [(k, v')]
combineValues = fmap2

-- Core Beam transform.
flatten :: [Stream e] -> Stream e
flatten = concat

-- Core Beam transform.
partition :: Integral i => i -> Stream e -> [Stream e]
partition n = fmap2 snd . fmap snd
            . groupBy ((`mod` n) . fst)
            . zip [0..]


-- Type of the computational graph node. It should be endomorphism
-- to be able to permute it with other nodes.
type Node e = Stream e -> Stream e
type Node2 e = Stream e -> Stream e -> Stream e

pardoNode :: (e -> [e]) -> Node e
pardoNode = pardo

reduceNode :: Ord k => (e -> k) -> ([e] -> [e]) -> Node e
reduceNode k reducer = pardo snd . combineValues reducer . groupBy k

coReduceNode :: Ord k => (k1, e -> k) -> (k2, e -> k)
             -> (k -> (k1, [e]) -> (k2, [e]) -> [e]) -> Node2 e
coReduceNode (k1, k) (k2, k') f es1 es2 =
  let es1' = pardo (\e -> [(k  e, e)]) es1
      es2' = pardo (\e -> [(k' e, e)]) es2
   in pardo (\(k, (kes1, kes2)) -> f k kes1 kes2) $ coGroupByKey (k1, es1') (k2, es2')
