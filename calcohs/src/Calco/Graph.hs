{-# LANGUAGE GADTs #-}

module Calco.Graph where

import           Calco.Defs
import           Data.Map   (Map)

type TermMarker = Integer

data Term =
    Const NodeName
  | App1 NodeName TermMarker
  | App2 NodeName TermMarker TermMarker
  deriving (Show)

newtype Graph = Graph (Map TermMarker Term)
