module Halco.Beam.Conts where

import           Data.Map        (Map)

import           Halco.Beam.Defs (Attr, Batch, Props)

data InMode = Process | Filter

data InCont = InCont
  { attrsI :: Map Attr Props
  , batchI :: Batch
  , mode   :: InMode }

data OutCont = OutCont
  { attrsO :: Map Attr Props
  , batchO :: Batch }
