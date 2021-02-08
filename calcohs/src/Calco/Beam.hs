{-# LANGUAGE GADTs #-}

module Calco.Beam where

import           Data.Map    (Map)
import qualified Data.Map    as Map

import           Calco.Defs  (Attr)
import           Calco.Utils (fmap2, insertOrApply)

type Stream e = [e]

pardo :: (e -> [e]) -> Stream e -> Stream e
pardo = concatMap

groupBy :: Ord k => (e -> k) -> Stream e -> [(k, [e])]
groupBy f = Map.toList . foldr g Map.empty
  where
    g e = insertOrApply (f e) [e] (e :)

groupBy' :: (Ord k, Ord k') => Map k (e -> k') -> [e] -> [(Map k k', [e])]
groupBy' ks = Map.toList . foldr f Map.empty
  where
    ap e = fmap ($ e) ks
    f e = insertOrApply (ap e) [e] (e :)

coGroupByKey :: Ord k => (k1, [(k, e1)]) -> (k2, [(k, e2)])
             -> [(k, ( (k1, [e1])
                     , (k2, [e2]) ))]
coGroupByKey (k1, es1) (k2, es2) =
  map from $ groupBy fst
    [(k, (e1, e2))
      | (k , e1) <- es1
      , (k', e2) <- es2
      , k == k']
  where
    from (k, es) = (k, ( (k1, map (fst . snd) es)
                       , (k2, map (snd . snd) es) ))

combinePerKey :: (Ord k, Semigroup e) => [(k, e)] -> [(k, e)]
combinePerKey = Map.toList . foldr f Map.empty
  where
    f (k, e) = insertOrApply k e (e <>)

combineValues :: (Ord k, Monoid e) => [(k, [e])] -> [(k, e)]
combineValues = combinePerKey . fmap2 mconcat

flatten :: [Stream e] -> Stream e
flatten = concat

partition :: Integral i => i -> Stream e -> [Stream e]
partition n = fmap2 snd . fmap snd
            . groupBy ((`mod` n) . fst)
            . zip [0..]
