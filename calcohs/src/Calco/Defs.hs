module Calco.Defs where

-- Sorts of information about the stream:
-- attributes that are available on each stream element;
-- properties that are satisfied for the attributes.
class (Show a, Eq a, Ord a) => Attr a
class (Show p, Eq p, Ord p) => Prop p

-- Name of the Graph node.
type NodeName = String
