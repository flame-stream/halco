module Calco.Defs where

class (Show a, Eq a, Ord a) => Attr a
class (Show p, Eq p, Ord p) => Prop p

type StreamName = String
type TfmName = String -- Transform name
