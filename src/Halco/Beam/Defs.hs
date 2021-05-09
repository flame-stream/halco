module Halco.Beam.Defs where

import           Data.Set (Set)

newtype Attr = Attr String
newtype Prop = Prop String

data Props = Props
  { local :: Set Prop
  , noloc :: Set Prop }

data Batch = None | Group Attr | CoGroup Attr
