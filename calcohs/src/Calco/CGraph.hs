{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Calco.CGraph where

import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Set          (Set)
import qualified Data.Set          as Set

import           Calco.Conts.Types (InCont, OutCont)
import           Calco.Defs        (NodeName)

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

env :: (InCont i, OutCont o)
    => [(NodeName, CStream o)]
    -> [(NodeName, CTfm1 i o)]
    -> [(NodeName, CTfm2 i o)]
    -> Env i o
env ss ts1 ts2 = Env { streams = Map.fromList ss
                     , tfms1   = Map.fromList ts1
                     , tfms2   = Map.fromList ts2 }

semantics :: [NodeName] -> Semantics
semantics = Set.fromList

streamC :: CStream o -> o
streamC (CStream o) = o

tfm1C :: CTfm1 i o -> (i, o)
tfm1C (CTfm1 i o) = (i, o)

tfm2C :: CTfm2 i o -> (i, i, o)
tfm2C (CTfm2 i1 i2 o) = (i1, i2, o)
