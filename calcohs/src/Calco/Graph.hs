{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Calco.Graph where

import           Calco.Defs
import           Data.Map   (Map)
import qualified Data.Map   as Map

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

cut :: Graph -> [TermId] -> Graph
cut = undefined
