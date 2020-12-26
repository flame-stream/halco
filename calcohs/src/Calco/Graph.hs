{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Calco.Graph where

import           Data.Function ((&))
import           Data.Map      (Map, (!))
import qualified Data.Map      as Map
import           Data.Set      (Set)
import qualified Data.Set      as Set

import           Calco.Defs
import           Calco.Utils

type TermId = Integer

data Term =
    Const NodeName
  | App1 NodeName TermId
  | App2 NodeName TermId TermId
  deriving (Show)

newtype Graph = Graph (Map TermId Term)
  deriving (Show)

graph :: [(TermId, Term)] -> Graph
graph = Graph . Map.fromList

getMap :: Graph -> Map TermId Term
getMap = \case Graph m -> m

nodeName :: Term -> NodeName
nodeName = \case
  Const nn    -> nn
  App1 nn _   -> nn
  App2 nn _ _ -> nn

nodeNames :: Graph -> [NodeName]
nodeNames (Graph m) = map nodeName $ Map.elems m

-- Cut correct (all needed term ids exist)
-- subgraph with provided terms
cut :: Graph -> Set TermId -> Graph
cut g@(Graph m) tids = Graph $ foldr f Map.empty $ Map.toList m
  where
    f :: (TermId, Term) -> Map TermId Term -> Map TermId Term
    f (tid, t) m'
      | tid `Map.member` m' = m'
      | tid `Set.notMember` tids = m'
      | otherwise = m' `Map.union` getMap (cut' g tid)

cut' :: Graph -> TermId -> Graph
cut' g@(Graph m) tid = m ! tid & Graph . \case
  c@(Const _) -> Map.singleton tid c
  a@(App1 _ tid') -> Map.insert tid a . getMap $ cut' g tid'
  a@(App2 _ tid1 tid2) ->
    let m1 = getMap $ cut' g tid1
        m2 = getMap $ cut' g tid2
     in Map.insert tid a $ m1 `Map.union` m2

findIds :: Graph -> NodeName -> [TermId]
findIds (Graph m) nn = findKeys nn $ nodeName <$> m
