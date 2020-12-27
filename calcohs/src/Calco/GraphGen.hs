{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Calco.GraphGen where

import           Control.Monad             (guard)
import qualified Control.Monad.State       as StateM
import           Control.Monad.Trans.Class (MonadTrans)
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
genGraphs (e@(Env m), s) =
  let ss = zip [1..] $ streams e
      consts = (Const <$>) <$> ss
      g = genFromSources 10 e
        (Set.fromList $ tfms e)
        (fmap (toState . stream e) <$> ss)
      tidMax = fst $ last ss
      bigGraph = graph $ consts ++ StateM.evalState (toList g) tidMax
   in map (bigGraph `cut`) $ semanticsTids bigGraph s
  where
    semanticsTids :: Graph -> Semantics -> [Set TermId]
    semanticsTids g = (Set.fromList <$>)
                    . cartesianProduct . (findIds g <$>)
                    . Set.toList

genFromSources :: ContContext a p i o
               => Int -> Env i o -> Set NodeName
               -> [(TermId, State a p)]
               -> ListT (StateM.State TermId) (TermId, Term)
genFromSources 0 _ _ _ = ListT $ pure Nothing
genFromSources depth e@(Env m) nns sources = do
  nn <- fromFoldable nns
  m ! nn & \case
    Stream _ -> error "Streams should be only in sources"
    Tfm1 i o -> do
      (tid, state) <- fromFoldable sources
      guard $ state `match` i
      updateTid
      tid' <- liftLT StateM.get
      (tid', App1 nn tid) `cons` genFromSources (depth - 1) e
        (nn `Set.delete` nns)
        ((tid', state `update` o) : sources)
    Tfm2 i1 i2 o -> do
      (tid1, state1) <- fromFoldable sources
      (tid2, state2) <- fromFoldable sources
      guard $ tid1 /= tid2
            && state1 `match` i1
            && state2 `match` i2
      updateTid
      tid' <- liftLT StateM.get
      (tid', App2 nn tid1 tid2) `cons` genFromSources (depth - 1) e
        (nn `Set.delete` nns)
        ((tid' , state1 `update` o) : sources)
  where
    updateTid = liftLT $ StateM.modify (+1)
