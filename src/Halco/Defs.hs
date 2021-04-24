module Halco.Defs where

-- Sorts of information about the stream:
-- attributes that are available on each stream element;
-- properties that are satisfied for the attributes.
class (Show a, Ord a, Eq a) => Attr a
class (Show p, Ord p, Eq p) => Prop p

-- Name of the Graph node.
type NodeName = String
