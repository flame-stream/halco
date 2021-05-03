module Halco.Trivial.Defs where

type AttrName = String
newtype Attr = Attr String
  deriving (Show, Eq, Ord)

type PropName = String
newtype Prop = Prop String
  deriving (Show, Eq, Ord)
