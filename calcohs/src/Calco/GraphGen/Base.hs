{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Calco.GraphGen.Base where

import           Control.Monad             (guard)
import qualified Control.Monad.State       as StateM
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Function             ((&))
import           Data.Map                  (Map, (!))
import qualified Data.Map                  as Map
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           ListT

import           Calco.CGraph
import           Calco.Conts
import           Calco.Defs
import           Calco.Graph
import           Calco.State
import           Calco.Utils

genGraphs :: ContContext a p i o => CGraph i o -> [Graph]
genGraphs (e, s) =
  let enumeratedStreams = zip [1..] $ streams e
      consts = Const <$$> enumeratedStreams
      terms = genFromSources 5 e
        (Set.fromList $ tfms e)
        (toState . stream e <$$> enumeratedStreams)
      tidMax = fst $ last enumeratedStreams
      bigGraph = graph $ consts ++ StateM.evalState (ListT.toList terms) tidMax
      graphs = map (bigGraph `extractPipeline`) $ semanticsTids bigGraph s
   in filter noSameNodes graphs -- Every semantics node also will occur only once.
  where
    semanticsTids :: Graph -> Semantics -> [SemanticIds]
    semanticsTids g = (Set.fromList <$>)
                    . cartesianProduct . (findIds g <$>)
                    . Set.toList

    noSameNodes :: Graph -> Bool
    noSameNodes = all (== (1 :: Integer)) . countOccs . nodeNames

genFromSources :: ContContext a p i o
               => Int                     -- Depth of the graph to generate.
               -> Env i o
               -> Set NodeName            -- Available transformations to use in graph.
               -> [(TermId, State a p)]   -- List of sources.
               -> ListT
                    (StateM.State TermId) -- Last used term id in graph.
                    (TermId, Term)
genFromSources 0 _ _ _ = nilLT
genFromSources depth e@(Env m) nns sources = do
  nn <- nnsLT
  m ! nn & \case
    Stream _ -> error "Streams should be only in sources"
    Tfm1 i o -> do
      (tid, state) <- sourcesLT
      guard $ state `match` i
      updateTid
      tid' <- getTid
      (tid', App1 nn tid) `cons` genFromSources (depth - 1) e
        (nn `Set.delete` nns)
        ((tid', state `update` o) : sources)
    Tfm2 i1 i2 o -> do
      (tid1, state1) <- sourcesLT
      (tid2, state2) <- sourcesLT
      guard $ tid1 /= tid2
            && state1 `match` i1
            && state2 `match` i2
      updateTid
      tid' <- getTid
      (tid', App2 nn tid1 tid2) `cons` genFromSources (depth - 1) e
        (nn `Set.delete` nns)
        ((tid' , (state1 <> state2) `update` o) : sources)
  where
    getTid = lift StateM.get
    updateTid = lift $ StateM.modify (+1)

    nnsLT = fromFoldable nns
    sourcesLT = fromFoldable sources
