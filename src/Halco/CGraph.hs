{-# LANGUAGE GADTs #-}

module Halco.CGraph where

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Set    (Set)
import qualified Data.Set    as Set

import           Halco.Conts (ContsContext, InCont, OutCont, OutCont1, OutCont2)
import           Halco.Defs  (NodeName)

data CSource s o where
  CSource :: OutCont s o => o -> CSource s o

data COp1 s i o where
  COp1 :: (InCont s i, OutCont1 s o) => i -> o -> COp1 s i o

data COp2 s i o where
  COp2 :: (InCont s i, OutCont2 s o) => i -> i -> o -> COp2 s i o

-- Set of nodes, annotated with contracts
data Env s i o o1 o2 where
  Env :: ContsContext s i o o1 o2
      => { sources :: Map NodeName (CSource s o)
         , ops1    :: Map NodeName (COp1 s i o1)
         , ops2    :: Map NodeName (COp2 s i o2) }
      -> Env s i o o1 o2

type Semantics = Set NodeName

type CGraph s i o o1 o2 = (Env s i o o1 o2, Semantics)

sourceC :: CSource s o -> o
sourceC (CSource o) = o

op1C :: COp1 s i o -> (i, o)
op1C (COp1 i o) = (i, o)

op2C :: COp2 s i o -> (i, i, o)
op2C (COp2 i1 i2 o) = (i1, i2, o)
