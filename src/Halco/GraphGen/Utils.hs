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
graphSources (CGraph.sources -> m) =
  ( enumerate $ toState . CGraph.sourceC . snd <$> Map.toList m
  , enumerate $ Source . fst <$> Map.toList m
  , toInteger $ Map.size m )
  where
    enumerate = zip [1..]
