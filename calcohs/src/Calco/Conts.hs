module Calco.Conts where

import Data.Set (Set)
import qualified Data.Set as Set


type StreamName = String
type AttrName = String
type PropName = String

data Attr = Attr StreamName AttrName
  deriving (Show, Ord, Eq)

data Prop = Prop PropName | AttrProp Attr PropName
  deriving (Show, Ord, Eq)


data InCont = InCont { attrsI :: Set Attr
                     , propsI :: Set Prop
                     , propsI' :: Set Prop }
  deriving (Show)

emptyInCont :: InCont
emptyInCont = InCont { attrsI = Set.empty
                     , propsI = Set.empty
                     , propsI' = Set.empty }


data DelAttrs = DelAllAttrs | DelAttrs (Set Attr)
  deriving (Show)

data OutCont = OutCont { attrsO :: Set Attr
                       , attrsO' :: DelAttrs
                       , propsO :: Set Prop
                       , propsO' :: Set Prop }
  deriving (Show)

emptyOutCont :: OutCont
emptyOutCont = OutCont { attrsO = Set.empty
                       , attrsO' = DelAttrs Set.empty
                       , propsO = Set.empty
                       , propsO' = Set.empty }
