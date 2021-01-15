{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Calco.Graph where

import           Data.Function ((&))
import           Data.Map      (Map, (!))
import qualified Data.Map      as Map
import           Data.Set      (Set)
import qualified Data.Set      as Set

import           Calco.CGraph  (Semantics)
import           Calco.Defs    (NodeName)
import           Calco.Utils   (cartesianProduct, countOccs, findKeys)

type TermId = Integer

type SemanticTids = Set TermId

data Term =
    Const NodeName
  | App1 NodeName TermId
  | App2 NodeName TermId TermId
  deriving (Show, Eq, Ord)

newtype Graph = Graph (Map TermId Term)
  deriving (Show)

graph :: [(TermId, Term)] -> Graph
graph = Graph . Map.fromList

toMap :: Graph -> Map TermId Term
toMap (Graph m) = m

toList :: Graph -> [(TermId, Term)]
toList = Map.toList . toMap

empty :: Graph
empty = graph []

union :: Graph -> Graph -> Graph
union (Graph m1) (Graph m2) = Graph $ m1 `Map.union` m2

nodeName :: Term -> NodeName
nodeName = \case
  Const nn    -> nn
  App1 nn _   -> nn
  App2 nn _ _ -> nn

nodeNames :: Graph -> [NodeName]
nodeNames (Graph m) = map nodeName $ Map.elems m

extractPipeline :: Graph -> SemanticTids -> Graph
extractPipeline g tids = Map.foldrWithKey f empty $ toMap g
  where
    f :: TermId -> Term -> Graph -> Graph
    f tid _ g'@(Graph m')
      | tid `Map.member` m' = g'
      | tid `Set.notMember` tids = g'
      | otherwise = g' `union` extractPipeline' g tid

extractPipeline' :: Graph -> TermId -> Graph
extractPipeline' g@(Graph m) tid = m ! tid & Graph . \case
  c@(Const _) -> Map.singleton tid c
  a@(App1 _ tid') -> Map.insert tid a . toMap $ extractPipeline' g tid'
  a@(App2 _ tid1 tid2) ->
    let m1 = toMap $ extractPipeline' g tid1
        m2 = toMap $ extractPipeline' g tid2
     in Map.insert tid a $ m1 `Map.union` m2

findIds :: Graph -> NodeName -> [TermId]
findIds (Graph m) nn = findKeys nn $ nodeName <$> m

semanticTids :: Graph -> Semantics -> [SemanticTids]
semanticTids g = (Set.fromList <$>)
                . cartesianProduct . (findIds g <$>)
                . Set.toList

noSameNodes :: Graph -> Bool
noSameNodes = all (== (1 :: Integer)) . countOccs . nodeNames
