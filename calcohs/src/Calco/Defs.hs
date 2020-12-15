module Calco.Defs where

class (Show a, Eq a, Ord a) => Attr a
class (Show p, Eq p, Ord p) => Prop p

-- Name of the Graph node. It may be stream or transformation (tfm)
type NodeName = String
