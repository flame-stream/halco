module Halco.ContImpls.Beam.Scheme where

import           Data.Map                  (Map)

import           Halco.ContImpls.Beam.Defs (Attr, Batch, Props)

data Scheme = Scheme
  { attrs :: Map Attr Props
  , batch :: Batch }
