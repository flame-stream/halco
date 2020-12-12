{-# LANGUAGE GADTs #-}

module Calco.Graph where

import           Calco.Defs
import           Data.Map   (Map)

type TermMarker = Integer

data Term =
    Const StreamName
  | App1 TfmName TermMarker
  | App2 TfmName TermMarker TermMarker
  deriving (Show)

newtype Graph = Graph (Map TermMarker Term)
