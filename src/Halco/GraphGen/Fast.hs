{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}

module Halco.GraphGen.Fast (genGraphs) where

import           Data.Map                 ((!))
import qualified Data.Map                 as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Tuple.Extra         (fst3)

import           Halco.CGraph             (CGraph, COp1 (COp1), COp2 (COp2),
                                           Env (..))
import qualified Halco.CGraph             as CGraph
import           Halco.Conts              (ContsContext, InCont (..),
                                           OutCont (..), OutCont1 (..),
                                           OutCont2 (..))
import           Halco.Defs               (NodeName)
import           Halco.Graph              (Graph, Node (..), NodeId, graph)
import qualified Halco.Graph              as Graph
import           Halco.Utils.Data.Functor ((<$$>))

genGraphs :: ContsContext s i o o1 o2 => CGraph s i o o1 o2 -> [Graph]
genGraphs (e, s) =
  let sourcesC = Map.toList $ CGraph.sources e in
  let nidMax = toInteger $ length sourcesC in
  let states = enumerate $ toState . CGraph.sourceC . snd <$> sourcesC in
  let sources = enumerate $ Source . fst <$> sourcesC in
  let nOps = Map.size (CGraph.ops1 e) + Map.size (CGraph.ops2 e) in
  let ops = genFromSources nOps e nidMax ((, Set.empty) <$> states) Set.empty in
  let bigGraph = graph $ sources ++ ops in
  let graphs = map (bigGraph `Graph.extractPipeline`) $ Graph.semanticNids bigGraph s in
  filter Graph.noSameNodes graphs -- Every semantics node also will occur only once
  where
    enumerate = zip [1..]

genFromSources :: ContsContext s i o o1 o2
               => Int                -- Graph depth.
               -> Env s i o o1 o2
               -> NodeId             -- Maximal used node id.
               -> [( (NodeId, s)     -- All available sources to make graph next layer.
                   , Set NodeName )] -- Particular transformations that were used
                                     -- to make up source.
               -> Set Node           -- Already generated nodes of transformations.
               -> [(NodeId, Node)]   -- nodes of the all possible graphs with the given depth.
genFromSources 0 _ _ _ _ = []
genFromSources d e nid sources nodes =
  let sources1 = zip
        [nid + 1..]
        [(op, s `update1` o, nn `Set.insert` nns)
          | ((nid, s), nns) <- sources
          , nn <- Map.keys $ CGraph.ops1 e
          , nn `Set.notMember` nns
          , let op = Op1 nn nid
          , op `Set.notMember` nodes
          , let COp1 i o = CGraph.ops1 e ! nn
          , s `match` i] in
  let nid' = if null sources1
        then nid
        else fst $ last sources1 in

  let sources2 = zip
        [nid' + 1..]
        [(op, (s1, s2) `update2` o, nn `Set.insert` (nns1 <> nns2))
          | ((nid1, s1), nns1) <- sources
          , ((nid2, s2), nns2) <- sources
          , nid1 /= nid2
          , nn <- Map.keys $ CGraph.ops2 e
          , nn `Set.notMember` nns1
          , nn `Set.notMember` nns2
          , let op = Op2 nn nid1 nid2
          , op `Set.notMember` nodes
          , let COp2 i1 i2 o = CGraph.ops2 e ! nn
          , s1 `match` i1
          , s2 `match` i2] in
  let nid'' = if null sources2
        then nid'
        else fst $ last sources2 in

  let nodes1 = fst3 <$$> sources1 in
  let nodes2 = fst3 <$$> sources2 in
  let sources' = sources ++ (toSource <$> sources1) ++ (toSource <$> sources2) in
  let nodes' = nodes <> Set.fromList (snd <$> nodes1) <> Set.fromList (snd <$> nodes2) in
  nodes1 ++ nodes2 ++ genFromSources (d - 1) e nid'' sources' nodes'
  where
    toSource (nid, (t, s, nns)) = ((nid, s), nns)
