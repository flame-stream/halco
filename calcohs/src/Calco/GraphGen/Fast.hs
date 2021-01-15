{-# LANGUAGE GADTs #-}

module Calco.GraphGen.Fast where

import qualified Control.Monad.State  as StateM
import           Data.Tuple.Extra     (uncurry3)
import           Debug.Trace

import           Calco.CGraph         (CGraph, Env (..), tfm1, tfm2, tfms1,
                                       tfms2)
import           Calco.Conts          (ContContext, match, update)
import           Calco.Graph          (Graph, Term (..), TermId,
                                       extractPipeline, graph, noSameNodes,
                                       semanticTids)
import           Calco.GraphGen.Utils (Source, graphSources)
import           Calco.State          (State)
import           Calco.Utils          ((<$$>))

genGraphs :: ContContext a p i o => CGraph i o -> [Graph]
genGraphs (e, s) =
  let (sources, consts, tidMax) = graphSources e
      terms = genFromSources 4 e tidMax sources
      bigGraph = graph $ consts ++ terms
      graphs = map (bigGraph `extractPipeline`) $ semanticTids bigGraph s
   in filter noSameNodes graphs -- Every semantics node also will occur only once.

genFromSources :: ContContext a p i o
               => Int              -- Graph depth.
               -> Env i o
               -> TermId           -- Maximal used term id.
               -> [Source a p]
               -> [(TermId, Term)]
genFromSources 0 _ _ _ = []
genFromSources d e tid sources = do -- TODO do for traceM
  let sources1 = zip
        [tid + 1..]
        [((nn, tid), s `update` o)
          | (tid, s) <- sources
          , nn <- tfms1 e
          , let (i, o) = tfm1 e nn
          , s `match` i]
      tid' = if null sources1
        then tid
        else fst $ last sources1

  traceM $ "tid' = " <> show tid'
        <> " d = " <> show d
        <> " len sources1 = " <> show (length sources1)

  let sources2 = zip
        [tid' + 1..]
        [((nn, tid1, tid2), (s1 <> s2) `update` o)
          | (tid1, s1) <- sources
          , (tid2, s2) <- sources
          , tid1 /= tid2
          , nn <- tfms2 e
          , let (i1, i2, o) = tfm2 e nn
          , s1 `match` i1
          , s2 `match` i2]
      tid'' = if null sources2
        then tid'
        else fst $ last sources2

  traceM $ "tid'' = " <> show tid''
        <> " d = " <> show d
        <> " len sources2 = " <> show (length sources2)

  let terms1 = uncurry App1 <$$> (fst <$$> sources1)
      terms2 = uncurry3 App2 <$$> (fst <$$> sources2)
      terms' = genFromSources (d - 1) e tid''
             $ sources ++ (snd <$$> sources1) ++ (snd <$$> sources2)
   in terms1 ++ terms2 ++ terms'
