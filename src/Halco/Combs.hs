module Halco.Combs where

import           Halco.Beam   (coGroupByKey, combineValues, groupBy, pardo)
import           Halco.EGraph (EOp1, EOp2, ESource)

-- Combinators that produce nodes with ability
-- to permute for computational graph

pardoNode :: (e -> [e]) -> EOp1 e
pardoNode = pardo

pardoNodeP :: (e -> Bool) -> EOp1 e
pardoNodeP p = pardoNode $ \e -> [e | p e]

reduceNode :: Ord k => (e -> k) -> ([e] -> [e]) -> EOp1 e
reduceNode k reducer = pardo snd . combineValues reducer . groupBy k

coReduceNode :: Ord k => (k1, e -> k) -> (k2, e -> k)
             -> (k -> (k1, [e]) -> (k2, [e]) -> [e]) -> EOp2 e
coReduceNode (k1, k) (k2, k') reducer es1 es2 =
  let es1' = pardo (\e -> [(k  e, e)]) es1 in
  let es2' = pardo (\e -> [(k' e, e)]) es2 in
  pardo (\(k, (kes1, kes2)) -> reducer k kes1 kes2)
      $ coGroupByKey (k1, es1') (k2, es2')

coReduceNode' :: (Ord k, Semigroup e) => (e -> k) -> (e -> k) -> EOp2 e
coReduceNode' k1 k2 = coReduceNode
  (undefined, k1) (undefined, k2)
  (\k (_, es1) (_, es2) -> (<>) <$> es1 <*> es2)
