module Halco.EGraph where

import           Data.Map    (Map, (!))
import qualified Data.Map    as Map

import           Halco.Defs  (NodeName)
import           Halco.Graph (Graph (Graph), Node (..), NodeId)
import qualified Halco.Graph as Graph

type ESource e = [e]
type EOp1 e = [e] -> [e]
type EOp2 e = [e] -> [e] -> [e]

-- Element types should be the same to be able to permute nodes
data Env' e = Env'
  { sources :: Map NodeName (ESource e)
  , ops1    :: Map NodeName (EOp1 e)
  , ops2    :: Map NodeName (EOp2 e) }

newtype EGraph e = EGraph (Map NodeName (ESource e))
  deriving (Show)

toMap :: EGraph e -> Map NodeName (ESource e)
toMap (EGraph m) = m

eval :: Graph -> Env' e -> EGraph e
eval g@(Graph m) e = EGraph $ Map.foldrWithKey f Map.empty m
  where
    f nid n m | Graph.nodeName n `Map.member` m = m
              | otherwise = eval' g e nid m

    eval' g@(Graph m) e nid m'
      | nodeName nid `Map.member` m' = m'
      | otherwise = case m ! nid of
        Source nn -> Map.insert nn (sources e ! nn) m'
        Op1 nn nid' ->
          let m' = eval' g e nid' m' in
          let s' = m' ! nodeName nid' in
          Map.insert nn (ops1 e ! nn $ s') m'
        Op2 nn nid1 nid2 ->
          let m1 = eval' g e nid1 m' in
          let s1 = m1 ! nodeName nid1 in
          let m2 = eval' g e nid2 m' in
          let s2 = m2 ! nodeName nid2 in
          Map.insert nn ((ops2 e ! nn) s1 s2) $ m1 <> m2
      where
        nodeName nid = Graph.nodeName $ m ! nid
