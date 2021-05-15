{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Halco.GraphGen.Base where

import           Control.Applicative       ((<|>))
import           Control.Monad             (guard)
import           Control.Monad.State       as State
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Function             ((&))
import           Data.Functor              ((<&>))
import           Data.Map                  (Map, (!), (!?))
import qualified Data.Map                  as Map
import           Data.Maybe                (fromJust)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           ListT

import           Halco.CGraph              (CGraph, COp1 (..), COp2 (..),
                                            Env (..))
import qualified Halco.CGraph              as CGraph
import           Halco.Conts
import           Halco.Defs                (NodeName)
import           Halco.Graph               (Graph (..), Node (..), NodeId,
                                            graph)
import qualified Halco.Graph               as Graph
import           Halco.Utils.ListT         (nilLT)

-- Very slow exponential algorithm, do not use it!
genGraphs :: ContsContext s i o o1 o2 => CGraph s i o o1 o2 -> [Graph]
genGraphs (e, s) =
  let sourcesC = Map.toList $ CGraph.sources e in
  let nidMax = toInteger $ length sourcesC in
  let schemes = enumerate $ toScheme . CGraph.sourceC . snd <$> sourcesC in
  let sources = enumerate $ Source . fst <$> sourcesC in
  let opsNames = Set.fromList $ Map.keys (CGraph.ops1 e) <> Map.keys (CGraph.ops2 e) in
  let nodes = genFromSources 5 e opsNames schemes in
  let bigGraph = graph $ sources ++ evalState (ListT.toList nodes) nidMax in
  let graphs = map (bigGraph `Graph.extractDataflow`) $ Graph.semanticNids bigGraph s in
  filter Graph.noSameNodes graphs -- Every semantics node also will occur only once
  where
    enumerate = zip [1..]

genFromSources :: ContsContext s i o o1 o2
               => Int                     -- Depth of the graph to generate.
               -> Env s i o o1 o2
               -> Set NodeName            -- Available transformations to use in graph.
               -> [(NodeId, s)]
               -> ListT
                    (State NodeId) -- Last used node id in graph.
                    (NodeId, Node)
genFromSources 0 _ _ _ = nilLT
genFromSources depth e nns sources = do
  nn <- nnsLT
  case CGraph.ops1 e !? nn of
    Just (COp1 i o) -> do
      (nid, scheme) <- sourcesLT
      guard $ scheme `match` i
      updateNid
      nid' <- getNid
      (nid', Op1 nn nid) `cons` genFromSources (depth - 1) e
        (nn `Set.delete` nns)
        ((nid', scheme `update1` o) : sources)
    Nothing -> CGraph.ops2 e ! nn & \case
      COp2 i1 i2 o -> do
        (nid1, scheme1) <- sourcesLT
        (nid2, scheme2) <- sourcesLT
        guard $ nid1 /= nid2
              && scheme1 `match` i1
              && scheme2 `match` i2
        updateNid
        nid' <- getNid
        (nid', Op2 nn nid1 nid2) `cons` genFromSources (depth - 1) e
          (nn `Set.delete` nns)
          ((nid', (scheme1, scheme2) `update2` o) : sources)
  where
    getNid = lift State.get
    updateNid = lift $ State.modify (+ 1)

    nnsLT = fromFoldable nns
    sourcesLT = fromFoldable sources
