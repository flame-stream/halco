module Halco.ContImpls.Pragmatic.Defs where

type AttrName = String
newtype Attr = Attr AttrName
  deriving (Show, Eq, Ord)

type PropName = String
newtype OpaqueProp = OpaqueProp PropName
  deriving (Show, Eq, Ord)

