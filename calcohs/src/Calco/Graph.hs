{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Calco.Graph where

import           Data.Map    (Map)
import qualified Data.Map    as Map

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

nodeName :: Term -> NodeName
nodeName = \case
  Const nn    -> nn
  App1 nn _   -> nn
  App2 nn _ _ -> nn

nodeNames :: Graph -> [NodeName]
nodeNames (Graph m) = map nodeName $ Map.elems m

-- Cut correct (all needed term ids exist)
-- subgraph with provided terms
cut :: Graph -> [TermId] -> Graph
cut = undefined

findIds :: Graph -> NodeName -> [TermId]
findIds (Graph m) nn = findKeys nn $ nodeName <$> m
