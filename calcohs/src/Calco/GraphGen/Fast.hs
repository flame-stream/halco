{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}

module Calco.GraphGen.Fast (genGraphs) where

import           Data.Map                 ((!))
import qualified Data.Map                 as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Tuple.Extra         (fst3)

import           Calco.CGraph             (CGraph, CTfm1 (CTfm1), CTfm2 (CTfm2),
                                           Env (..))
import qualified Calco.CGraph             as CGraph
import           Calco.Conts.Types        (ContContext, InCont (..),
                                           OutCont (..))
import           Calco.Defs               (NodeName)
import           Calco.Graph              (Graph, Node (..), NodeId, graph)
import qualified Calco.Graph              as Graph
import           Calco.GraphGen.Utils     (graphSources)
import           Calco.State              (State)
import           Calco.Utils.Data.Functor ((<$$>))

genGraphs :: ContContext a p i o => CGraph i o -> [Graph]
genGraphs (e, s) =
  let (sources, streams, nidMax) = graphSources e
      nTfms = Map.size (CGraph.tfms1 e) + Map.size (CGraph.tfms2 e)
      tfms = genFromSources nTfms e nidMax ((, Set.empty) <$> sources) Set.empty
      bigGraph = graph $ streams ++ tfms
      graphs = map (bigGraph `Graph.extractPipeline`) $ Graph.semanticNids bigGraph s
   in filter Graph.noSameNodes graphs -- Every semantics node also will occur only once.

genFromSources :: ContContext a p i o
               => Int                    -- Graph depth.
               -> Env i o
               -> NodeId                 -- Maximal used node id.
               -> [( (NodeId, State a p) -- All available sources to make graph next layer.
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
