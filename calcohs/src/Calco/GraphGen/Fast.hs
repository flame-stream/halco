{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}

module Calco.GraphGen.Fast where

import qualified Control.Monad.State      as StateM
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Debug.Trace

import           Calco.CGraph             (CGraph, Env (..), tfm1, tfm2, tfms,
                                           tfms1, tfms2)
import           Calco.Conts.Types        (ContContext, match, update)
import           Calco.Defs               (NodeName)
import           Calco.Graph              (Graph, Term (..), TermId,
                                           extractPipeline, graph, noSameNodes,
                                           nodeName, semanticTids)
import           Calco.GraphGen.Utils     (Source, graphSources)
import           Calco.State              (State)
import           Calco.Utils.Data.Functor ((<$$>))
import           Data.Tuple.Extra         (fst3)

genGraphs :: ContContext a p i o => CGraph i o -> [Graph]
genGraphs (e, s) =
  let (sources, consts, tidMax) = graphSources e
      nTfms = length $ tfms e
      apps = genFromSources nTfms e tidMax ((, Set.empty) <$> sources) Set.empty
      bigGraph = graph $ consts ++ apps
      graphs = map (bigGraph `extractPipeline`) $ semanticTids bigGraph s
   in filter noSameNodes graphs -- Every semantics node also will occur only once.

genFromSources :: ContContext a p i o
               => Int                 -- Graph depth.
               -> Env i o
               -> TermId              -- Maximal used term id.
               -> [( Source a p       -- All available sources to make graph next layer.
                   , Set NodeName )]  -- Particular transformations that were used
                                      -- to make up source.
               -> Set Term            -- Already generated terms of transformations.
               -> [(TermId, Term)]    -- Terms of the all possible graphs with the given depth.
genFromSources 0 _ _ _ _ = []
genFromSources d e tid sources terms =
  let sources1 = zip
        [tid + 1..]
        [(app, s `update` o, nn `Set.insert` nns)
          | ((tid, s), nns) <- sources
          , nn <- tfms1 e
          , nn `Set.notMember` nns
          , let app = App1 nn tid
          , app `Set.notMember` terms
          , let (i, o) = tfm1 e nn
          , s `match` i]
      tid' = if null sources1
        then tid
        else fst $ last sources1

      sources2 = zip
        [tid' + 1..]
        [(app, (s1 <> s2) `update` o, nn `Set.insert` (nns1 <> nns2))
          | ((tid1, s1), nns1) <- sources
          , ((tid2, s2), nns2) <- sources
          , tid1 /= tid2
          , nn <- tfms2 e
          , nn `Set.notMember` nns1
          , nn `Set.notMember` nns2
          , let app = App2 nn tid1 tid2
          , app `Set.notMember` terms
          , let (i1, i2, o) = tfm2 e nn
          , s1 `match` i1
          , s2 `match` i2]
      tid'' = if null sources2
        then tid'
        else fst $ last sources2

      terms1 = fst3 <$$> sources1
      terms2 = fst3 <$$> sources2
      sources' = sources ++ (toSource <$> sources1) ++ (toSource <$> sources2)
      terms' = terms <> Set.fromList (snd <$> terms1) <> Set.fromList (snd <$> terms2)
   in terms1 ++ terms2 ++ genFromSources (d - 1) e tid'' sources' terms'
  where
    toSource (tid, (t, s, nns)) = ((tid, s), nns)
