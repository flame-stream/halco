{-# LANGUAGE GADTs        #-}
{-# LANGUAGE ViewPatterns #-}

module Halco.GraphGen.Utils where

import qualified Data.Map          as Map

import           Halco.CGraph      (Env (..))
import qualified Halco.CGraph      as CGraph
import           Halco.Conts.Types (ContContext, OutCont (..))
import           Halco.Graph       (Node (..), NodeId)
import           Halco.State       (State)

graphSources :: ContContext a p i o
             => Env i o -> ([(NodeId, State a p)], [(NodeId, Node)], NodeId)
graphSources (CGraph.streams -> m) =
  let sources = toState . CGraph.streamC . snd <$> Map.toList m
      streams = Stream . fst <$> Map.toList m
      nidMax = toInteger $ Map.size m
   in (enumerate sources, enumerate streams, nidMax)
  where
    enumerate = zip [1..]
