{-# LANGUAGE GADTs #-}

module Calco.GraphGen where

import           Data.Map     (Map, (!))
import qualified Data.Map     as Map
import           Data.Set     (Set)
import qualified Data.Set     as Set

import           Calco.CGraph
import           Calco.Conts
import           Calco.Defs
import           Calco.Graph
import           Calco.State

genGraphs :: ContContext a p i o => CGraph i o -> [Graph]
genGraphs (e@(Env m), s) = undefined
--   let ss = streams e
--       tms = (nnToState <$>) <$> zip [1..] ss
--       tms' = genGraphsHelper 10 e (Set.fromList $ tfms e) 0 tms
--       bigGraph = Graph $ Map.fromList tms'
--    in undefined
--   where
--     nnToState = update empty . stream e

-- genGraphsHelper :: ContContext a p i o
--                 => Int -> Env i o -> Set NodeName
--                 -> TermId -> [(TermId, State a p)]
--                 -> [(TermId, Term)]
-- genGraphsHelper 0 _ _ _ _ = []
-- genGraphsHelper depth e@(Env m) nns tmMax tms = do
--   nn <- Set.toList nns
--   let node = m ! nn
--   undefined
