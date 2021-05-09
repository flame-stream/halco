module Halco.ContImpls.Beam.State where

import           Data.Map                  (Map)

import           Halco.ContImpls.Beam.Defs (Attr, Batch, Props)

data State = State
  { attrs :: Map Attr Props
  , batch :: Batch }
