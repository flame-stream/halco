{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}

module Halco.GraphGen.Fast (genGraphs) where

import           Data.Map                 ((!))
import qualified Data.Map                 as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Tuple.Extra         (fst3)

import           Halco.CGraph             (CGraph, CTfm1 (CTfm1), CTfm2 (CTfm2),
                                           Env (..))
import qualified Halco.CGraph             as CGraph
import           Halco.Conts              (InCont (..), OutCont (..))
import           Halco.Defs               (NodeName)
import           Halco.Graph              (Graph, Node (..), NodeId, graph)
import qualified Halco.Graph              as Graph
import           Halco.Utils.Data.Functor ((<$$>))

genGraphs :: (InCont s i, OutCont s o) => CGraph s i o -> [Graph]
genGraphs (e, s) =
  let sourcesC = Map.toList $ CGraph.sources e in
  let nidMax = toInteger $ length sourcesC in
  let states = enumerate $ toState . CGraph.sourceC . snd <$> sourcesC in
  let sources = enumerate $ Source . fst <$> sourcesC in
  let nTfms = Map.size (CGraph.tfms1 e) + Map.size (CGraph.tfms2 e) in
  let tfms = genFromSources nTfms e nidMax ((, Set.empty) <$> states) Set.empty in
  let bigGraph = graph $ sources ++ tfms in
  let graphs = map (bigGraph `Graph.extractPipeline`) $ Graph.semanticNids bigGraph s in
  filter Graph.noSameNodes graphs -- Every semantics node also will occur only once
  where
    enumerate = zip [1..]

genFromSources :: (InCont s i, OutCont s o)
               => Int                    -- Graph depth.
               -> Env s i o
               -> NodeId                 -- Maximal used node id.
               -> [( (NodeId, s)         -- All available sources to make graph next layer.
                   , Set NodeName )]     -- Particular transformations that were used
                                         -- to make up source.
               -> Set Node               -- Already generated nodes of transformations.
               -> [(NodeId, Node)]       -- nodes of the all possible graphs with the given depth.
genFromSources 0 _ _ _ _ = []
genFromSources d e nid sources nodes =
  let sources1 = zip
        [nid + 1..]
        [(tfm, s `update` o, nn `Set.insert` nns)
          | ((nid, s), nns) <- sources
          , nn <- Map.keys $ CGraph.tfms1 e
          , nn `Set.notMember` nns
          , let tfm = Tfm1 nn nid
          , tfm `Set.notMember` nodes
          , let CTfm1 i o = CGraph.tfms1 e ! nn
          , s `match` i]
      nid' = if null sources1
        then nid
        else fst $ last sources1

      sources2 = zip
        [nid' + 1..]
        [(tfm, (s1 <> s2) `update` o, nn `Set.insert` (nns1 <> nns2))
          | ((nid1, s1), nns1) <- sources
          , ((nid2, s2), nns2) <- sources
          , nid1 /= nid2
          , nn <- Map.keys $ CGraph.tfms2 e
          , nn `Set.notMember` nns1
          , nn `Set.notMember` nns2
          , let tfm = Tfm2 nn nid1 nid2
          , tfm `Set.notMember` nodes
          , let CTfm2 i1 i2 o = CGraph.tfms2 e ! nn
          , s1 `match` i1
          , s2 `match` i2]
      nid'' = if null sources2
        then nid'
        else fst $ last sources2

      nodes1 = fst3 <$$> sources1
      nodes2 = fst3 <$$> sources2
      sources' = sources ++ (toSource <$> sources1) ++ (toSource <$> sources2)
      nodes' = nodes <> Set.fromList (snd <$> nodes1) <> Set.fromList (snd <$> nodes2)
   in nodes1 ++ nodes2 ++ genFromSources (d - 1) e nid'' sources' nodes'
  where
    toSource (nid, (t, s, nns)) = ((nid, s), nns)
