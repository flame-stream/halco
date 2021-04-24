module Halco.Defs where

-- Sorts of information about the dataflow:
-- attributes that are available on each dataflow element;
-- properties that are satisfied for the attributes.
class (Show a, Ord a, Eq a) => Attr a
class (Show p, Ord p, Eq p) => Prop p

type NodeName = String
