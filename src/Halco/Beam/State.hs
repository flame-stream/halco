module Halco.Beam.State where

import           Data.Map        (Map)

import           Halco.Beam.Defs (Attr, Batch, Props)

data State = State
  { attrs :: Map Attr Props
  , batch :: Batch }
