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
import           Halco.Utils.Data.List    (length')

genGraphs :: ContsContext s i o o1 o2 => CGraph s i o o1 o2 -> [Graph]
genGraphs (e, s) =
  let sourceConts = Map.toList $ CGraph.sources e in
  let nidMax = length' sourceConts in
  let sourceStates = enumerate $ toState . CGraph.sourceC . snd <$> sourceConts in
  let sources = enumerate $ Source . fst <$> sourceConts in
  let depth = Map.size (CGraph.ops1 e) + Map.size (CGraph.ops2 e) in
  let ops = genFromSources depth e nidMax ((, Set.empty) <$> sourceStates) Set.empty in
  let bigGraph = graph $ sources ++ ops in
  let graphs = map (bigGraph `Graph.extractDataflow`) $ Graph.semanticNids bigGraph s in
  filter Graph.noSameNodes graphs -- Every semantics node also will occur only once
  where
    enumerate = zip [1..]

genFromSources :: ContsContext s i o o1 o2
               => Int                -- Generation depth
               -> Env s i o o1 o2
               -> NodeId             -- Maximal used node id
               -> [( (NodeId, s)     -- All available sources to generate next layer
                   , Set NodeName )] -- Particular transformations that were used
                                     -- to make up source
               -> Set Node           -- Already generated operation nodes
               -> [(NodeId, Node)]   -- Nodes of the all possible graphs
genFromSources 0 _ _ _ _ = []
genFromSources d e nid sources nodes =
  let ops1 =
        [ (op, s `update1` o, nn `Set.insert` nns)
        | ((nid, s), nns) <- sources
        , (nn, COp1 i o) <- Map.toList $ CGraph.ops1 e
        , s `match` i
        , nn `Set.notMember` nns
        , let op = Op1 nn nid
        , op `Set.notMember` nodes ] in
  let enumerate :: [a] -> [(NodeId, a)]
      enumerate = zip [nid + 1..] in
  let ops1' = enumerate ops1 in
  let nid' = nid + length' ops1 in

  let ops2 =
        [ (op, (s1, s2) `update2` o, nn `Set.insert` (nns1 <> nns2))
        | ((nid1, s1), nns1) <- sources
        , ((nid2, s2), nns2) <- sources
        , nid1 /= nid2
        , (nn, COp2 i1 i2 o) <- Map.toList $ CGraph.ops2 e
        , s1 `match` i1
        , s2 `match` i2
        , nn `Set.notMember` nns1
        , nn `Set.notMember` nns2
        , let op = Op2 nn nid1 nid2
        , op `Set.notMember` nodes ] in
  let enumerate' :: [a] -> [(NodeId, a)]
      enumerate' = zip [nid' + 1..] in
  let ops2' = enumerate' ops2 in
  let nid'' = nid' + length' ops2 in

  let nodes1 = fst3 <$$> ops1' in
  let nodes2 = fst3 <$$> ops2' in
  let sources' = sources ++ (toSource <$> ops1') ++ (toSource <$> ops2') in
  let nodes' = nodes <> Set.fromList (fst3 <$> ops1) <> Set.fromList (fst3 <$> ops2) in
  nodes1 ++ nodes2 ++ genFromSources (d - 1) e nid'' sources' nodes'
  where
    toSource (nid, (_, s, nns)) = ((nid, s), nns)
