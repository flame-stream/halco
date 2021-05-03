{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Halco.CGraph where

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Set    (Set)
import qualified Data.Set    as Set

import           Halco.Conts (InCont, OutCont)
import           Halco.Defs  (NodeName)

data CSource s o where
  CSource :: OutCont s o => o -> CSource s o

data CTfm1 s i o where
  CTfm1 :: (InCont s i, OutCont s o) => i -> o -> CTfm1 s i o

data CTfm2 s i o where
  CTfm2 :: (InCont s i, OutCont s o) => i -> i -> o -> CTfm2 s i o

-- Set of nodes, annotated with contracts
data Env s i o where
  Env :: (InCont s i, OutCont s o)
      => { sources :: Map NodeName (CSource s o)
         , tfms1   :: Map NodeName (CTfm1 s i o)
         , tfms2   :: Map NodeName (CTfm2 s i o)
         }
      -> Env s i o

type Semantics = Set NodeName

type CGraph s i o = (Env s i o, Semantics)

sourceC :: CSource s o -> o
sourceC (CSource o) = o

tfm1C :: CTfm1 s i o -> (i, o)
tfm1C (CTfm1 i o) = (i, o)

tfm2C :: CTfm2 s i o -> (i, i, o)
tfm2C (CTfm2 i1 i2 o) = (i1, i2, o)
