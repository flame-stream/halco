{-# LANGUAGE GADTs        #-}
{-# LANGUAGE ViewPatterns #-}

module Calco.GraphGen.Utils where

import qualified Data.Map          as Map

import           Calco.CGraph      (Env (..))
import qualified Calco.CGraph      as CGraph
import           Calco.Conts.Types (ContContext, OutCont (..))
import           Calco.Graph       (Node (..), NodeId)
import           Calco.State       (State)

graphSources :: ContContext a p i o
             => Env i o -> ([(NodeId, State a p)], [(NodeId, Node)], NodeId)
graphSources (CGraph.streams -> m) =
  let sources = toState . CGraph.streamC . snd <$> Map.toList m
      streams = Stream . fst <$> Map.toList m
      nidMax = toInteger $ Map.size m
   in (enumerate sources, enumerate streams, nidMax)
  where
    enumerate = zip [1..]
