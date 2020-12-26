{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Calco.GraphGen where

import           Control.Monad (guard)
import           Data.Function ((&))
import           Data.Map      (Map, (!))
import qualified Data.Map      as Map
import           Data.Set      (Set)
import qualified Data.Set      as Set

import           Calco.CGraph
import           Calco.Conts
import           Calco.Defs
import           Calco.Graph
import           Calco.State
import           Calco.Utils

genGraphs :: ContContext a p i o => CGraph i o -> [Graph]
genGraphs (e@(Env m), s) =
  let ss = zip [1..] $ streams e
      consts = (Const <$>) <$> ss
      bigGraph = graph $ consts ++ genFromSources 10 e
        (Set.fromList $ tfms e)
        (fst $ last ss)
        (fmap (toState . stream e) <$> ss)
   in map (bigGraph `cut`) $ semanticsTids bigGraph s
  where
    semanticsTids :: Graph -> Semantics -> [Set TermId]
    semanticsTids g = (Set.fromList <$>)
                    . cartesianProduct . (findIds g <$>)
                    . Set.toList

genFromSources :: ContContext a p i o
               => Int -> Env i o -> Set NodeName
               -> TermId -> [(TermId, State a p)]
               -> [(TermId, Term)]  -- TODO monad transformer with Writer to store tidMax
genFromSources 0 _ _ _ _ = []
genFromSources depth e@(Env m) nns tidMax sources = do
  nn <- Set.toList nns
  m ! nn & \case
    Stream _ -> error "Streams should be in sources"
    Tfm1 i o -> do
      (tid, state) <- sources
      guard $ state `match` i
      (undefined, App1 nn tid) : genFromSources (depth - 1) e
        (nn `Set.delete` nns)
        undefined
        ((undefined, state `update` o) : sources)
    Tfm2 i1 i2 o -> do
      (tid1, state1) <- sources
      (tid2, state2) <- sources
      guard $ tid1 /= tid2
            && state1 `match` i1
            && state2 `match` i2
      (undefined, App2 nn tid1 tid2) : genFromSources (depth - 1) e
        (nn `Set.delete` nns)
        undefined
        ((undefined , state1 `update` o) : sources)
