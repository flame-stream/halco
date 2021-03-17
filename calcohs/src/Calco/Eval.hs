module Calco.Eval where

import           Data.Map    (Map, (!))
import qualified Data.Map    as Map

import           Calco.Defs  (NodeName)
import           Calco.Graph (Graph (Graph), Node (..), NodeId)
import qualified Calco.Graph as Graph

-- Element types should be the same to be able to permute nodes.
type EStream e = [e]
type ETfm1 e = EStream e -> EStream e
type ETfm2 e = EStream e -> EStream e -> EStream e

data Env' e = Env'
  { streams :: Map NodeName (EStream e)
  , tfms1   :: Map NodeName (ETfm1 e)
  , tfms2   :: Map NodeName (ETfm2 e)
  }

type Result e = Map NodeName (EStream e)

eval :: Graph -> Env' e -> Result e
eval g@(Graph m) e = Map.foldrWithKey f Map.empty m
  where
    -- f :: NodeId -> Node -> Result e -> Result e
    f nid n r | Graph.nodeName n `Map.member` r = r
              | otherwise = eval' g e nid r

    -- eval' :: Graph -> Env' e -> NodeId -> Result e -> Result e
    eval' g@(Graph m) e nid r
      | nodeName nid `Map.member` r = r
      | otherwise = case m ! nid of
        Stream nn -> Map.insert nn (streams e ! nn) r
        Tfm1 nn nid' ->
          let m' = eval' g e nid' r
              s' = m' ! nodeName nid'
           in Map.insert nn (tfms1 e ! nn $ s') m'
        Tfm2 nn nid1 nid2 ->
          let m1 = eval' g e nid1 r
              s1 = m1 ! nodeName nid1
              m2 = eval' g e nid2 r
              s2 = m2 ! nodeName nid2
           in Map.insert nn ((tfms2 e ! nn) s1 s2) $ m1 <> m2
      where
        nodeName nid = Graph.nodeName $ m ! nid
