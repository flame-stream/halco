{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Calco.Graph where

import           Calco.Defs
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Set   (Set)
import qualified Data.Set   as Set

type TermMarker = Integer

data Term =
    Const NodeName
  | App1 NodeName TermMarker
  | App2 NodeName TermMarker TermMarker
  deriving (Show)

newtype Graph = Graph (Map TermMarker Term)
  deriving (Show)

nodeNames :: Graph -> Set NodeName
nodeNames (Graph m) = flip foldMap m
                    $ Set.singleton . \case
  Const nn    ->  nn
  App1 nn _   -> nn
  App2 nn _ _ -> nn

graph :: [(TermMarker, Term)] -> Graph
graph = Graph . Map.fromList
