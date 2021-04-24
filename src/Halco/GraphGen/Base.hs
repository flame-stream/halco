{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Halco.GraphGen.Base where

import           Control.Applicative       ((<|>))
import           Control.Monad             (guard)
import qualified Control.Monad.State       as StateM
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Function             ((&))
import Data.Functor ((<&>))
import           Data.Map                  (Map, (!), (!?))
import qualified Data.Map                  as Map
import           Data.Maybe                (fromJust)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           ListT

import           Halco.CGraph              (CGraph, CTfm1 (..), CTfm2 (..),
                                            Env (..))
import qualified Halco.CGraph              as CGraph
import           Halco.Conts.Types
import           Halco.Defs                (NodeName)
import           Halco.Graph               (Graph (..), Node (..), NodeId,
                                            graph)
import qualified Halco.Graph               as Graph
import           Halco.GraphGen.Utils      (graphSources)
import           Halco.State               (State)
import           Halco.Utils.ListT         (nilLT)

-- Very slow exponential algorithm, do not use it!
genGraphs :: ContContext a p i o => CGraph i o -> [Graph]
genGraphs (e, s) =
  let (sources, streams, nidMax) = graphSources e
      tfmsNames = Set.fromList $ Map.keys (CGraph.tfms1 e) <> Map.keys (CGraph.tfms2 e)
      nodes = genFromSources 5 e tfmsNames sources
      bigGraph = graph $ streams ++ StateM.evalState (ListT.toList nodes) nidMax
      graphs = map (bigGraph `Graph.extractPipeline`) $ Graph.semanticNids bigGraph s
   in filter Graph.noSameNodes graphs -- Every semantics node also will occur only once.

genFromSources :: ContContext a p i o
               => Int                     -- Depth of the graph to generate.
               -> Env i o
               -> Set NodeName            -- Available transformations to use in graph.
               -> [(NodeId, State a p)]
               -> ListT
                    (StateM.State NodeId) -- Last used node id in graph.
                    (NodeId, Node)
genFromSources 0 _ _ _ = nilLT
genFromSources depth e nns sources = do
  nn <- nnsLT
  case CGraph.tfms1 e !? nn of
    Just (CTfm1 i o) -> do
      (nid, state) <- sourcesLT
      guard $ state `match` i
      updateNid
      nid' <- getNid
      (nid', Tfm1 nn nid) `cons` genFromSources (depth - 1) e
        (nn `Set.delete` nns)
        ((nid', state `update` o) : sources)
    Nothing -> CGraph.tfms2 e ! nn & \case
      CTfm2 i1 i2 o -> do
        (nid1, state1) <- sourcesLT
        (nid2, state2) <- sourcesLT
        guard $ nid1 /= nid2
              && state1 `match` i1
              && state2 `match` i2
        updateNid
        nid' <- getNid
        (nid', Tfm2 nn nid1 nid2) `cons` genFromSources (depth - 1) e
          (nn `Set.delete` nns)
          ((nid', (state1 <> state2) `update` o) : sources)
  where
    getNid = lift StateM.get
    updateNid = lift $ StateM.modify (+ 1)

    nnsLT = fromFoldable nns
    sourcesLT = fromFoldable sources
