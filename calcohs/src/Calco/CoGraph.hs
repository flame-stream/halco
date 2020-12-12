{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}

module Calco.CoGraph where

import           Calco.Conts
import           Calco.Defs
import           Data.Map    (Map)
import           Data.Set    (Set)

type Streams i = Map StreamName i
type Tfms1 i o = Map TfmName (i, o)
type Tfms2 i o = Map TfmName (i, i, o)

type EnvContext i o = (InCont i, OutCont o)

-- Set streams and transformations, annotated with contracts
data Env i o where
  Env :: EnvContext i o
      => { streams :: Streams o
         , tfms1   :: Tfms1 i o
         , tfms2   :: Tfms2 i o
         }                      -> Env i o

type Semantics = Set TfmName

type CoGraph i o = (Env i o, Semantics)
