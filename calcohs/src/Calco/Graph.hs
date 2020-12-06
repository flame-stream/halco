module Calco.Graph where

import Data.Map (Map)
import Data.Set (Set)
import Calco.Conts


-- Transform name
type TfmName = String

type Streams = Map StreamName OutCont
type Tfms1 = Map TfmName (InCont, OutCont)
type Tfms2 = Map TfmName (InCont, InCont, OutCont)

data Env = Env { streams :: Streams
               , tfms1 :: Tfms1
               , tfms2 :: Tfms2 }
  deriving (Show)

type Semantics = Set TfmName


type TermMarker = Integer

data Term = Const StreamName
          | App1 TfmName TermMarker
          | App2 TfmName TermMarker TermMarker
  deriving (Show)

newtype Graph = Graph (Map TermMarker Term)
