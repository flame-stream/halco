module Calco.EGraph where

import           Data.Map    (Map, (!))
import qualified Data.Map    as Map

import           Calco.Defs  (NodeName)
import           Calco.Graph (Graph (Graph), Node (..), NodeId)
import qualified Calco.Graph as Graph

type EStream e = [e]
type ETfm1 e = EStream e -> EStream e
type ETfm2 e = EStream e -> EStream e -> EStream e

-- Element types should be the same to be able to permute nodes.
data Env' e = Env'
  { streams :: Map NodeName (EStream e)
  , tfms1   :: Map NodeName (ETfm1 e)
  , tfms2   :: Map NodeName (ETfm2 e)
  }

newtype EGraph e = EGraph (Map NodeName (EStream e))

toMap :: EGraph e -> Map NodeName (EStream e)
toMap (EGraph m) = m

eval :: Graph -> Env' e -> EGraph e
eval g@(Graph m) e = EGraph $ Map.foldrWithKey f Map.empty m
  where
    f nid n m | Graph.nodeName n `Map.member` m = m
              | otherwise = eval' g e nid m

    eval' g@(Graph m) e nid m'
      | nodeName nid `Map.member` m' = m'
      | otherwise = case m ! nid of
        Stream nn -> Map.insert nn (streams e ! nn) m'
        Tfm1 nn nid' ->
          let m' = eval' g e nid' m'
              s' = m' ! nodeName nid'
           in Map.insert nn (tfms1 e ! nn $ s') m'
        Tfm2 nn nid1 nid2 ->
          let m1 = eval' g e nid1 m'
              s1 = m1 ! nodeName nid1
              m2 = eval' g e nid2 m'
              s2 = m2 ! nodeName nid2
           in Map.insert nn ((tfms2 e ! nn) s1 s2) $ m1 <> m2
      where
        nodeName nid = Graph.nodeName $ m ! nid