{-# LANGUAGE GADTs #-}

module Calco.Beam where

import           Data.Map                 (Map)
import qualified Data.Map                 as Map

import           Calco.Defs               (Attr)
import           Calco.Utils.Data.Functor (fmap2, (<$$>))
import           Calco.Utils.Data.Map     (insertOrApply)

type Stream e = [e]

pardo :: (e -> [e]) -> Stream e -> Stream e
pardo = concatMap

groupBy :: Ord k => (e -> k) -> Stream e -> [(k, [e])]
groupBy f = Map.toList . foldr g Map.empty
  where
    g e = insertOrApply (f e) [e] (e :)

-- Group by a different number of named properties.
groupBy' :: (Ord k, Ord k') => Map k (e -> k') -> Stream e -> [(Map k k', [e])]
groupBy' ks = Map.toList . foldr f Map.empty
  where
    ap e = fmap ($ e) ks
    f e = insertOrApply (ap e) [e] (e :)

-- Apply relational join to two streams.
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

combinePerKey :: (Ord k, Semigroup v) => [(k, v)] -> [(k, v)]
combinePerKey = Map.toList . foldr f Map.empty
  where
    f (k, e) = insertOrApply k e (e <>)

combineValues :: (Ord k, Monoid v) => [(k, [v])] -> [(k, v)]
combineValues = combinePerKey . fmap2 mconcat

flatten :: [Stream e] -> Stream e
flatten = concat

partition :: Integral i => i -> Stream e -> [Stream e]
partition n = fmap2 snd . fmap snd
            . groupBy ((`mod` n) . fst)
            . zip [0..]
