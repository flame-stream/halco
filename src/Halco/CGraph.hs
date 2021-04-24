{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Halco.CGraph where

import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Set          (Set)
import qualified Data.Set          as Set

import           Halco.Conts.Types (InCont, OutCont)
import           Halco.Defs        (NodeName)

-- TODO rename to source
data CStream o where
  CStream :: OutCont o => o -> CStream o

data CTfm1 i o where
  CTfm1 :: (InCont i, OutCont o) => i -> o -> CTfm1 i o

data CTfm2 i o where
  CTfm2 :: (InCont i, OutCont o) => i -> i -> o -> CTfm2 i o

-- Set of nodes, annotated with contracts.
data Env i o where
  Env :: (InCont i, OutCont o)
      => { streams :: Map NodeName (CStream o)
         , tfms1   :: Map NodeName (CTfm1 i o)
         , tfms2   :: Map NodeName (CTfm2 i o)
         }
      -> Env i o

type Semantics = Set NodeName

type CGraph i o = (Env i o, Semantics)

streamC :: CStream o -> o
streamC (CStream o) = o

tfm1C :: CTfm1 i o -> (i, o)
tfm1C (CTfm1 i o) = (i, o)

tfm2C :: CTfm2 i o -> (i, i, o)
tfm2C (CTfm2 i1 i2 o) = (i1, i2, o)
