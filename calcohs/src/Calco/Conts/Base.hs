{-# LANGUAGE TypeFamilies #-}

module Calco.Conts.Base where

import           Data.Set    (Set, (\\))
import qualified Data.Set    as Set

import qualified Calco.Conts as Conts
import           Calco.Defs  (NodeName)
import qualified Calco.Defs  as Defs
import           Calco.State (State (State), attrs, props)

type AttrName = String
type PropName = String

newtype Attr = Attr AttrName
  deriving (Show, Ord, Eq)

instance Defs.Attr Attr

newtype Prop = Prop PropName
  deriving (Show, Ord, Eq)

instance Defs.Prop Prop

data InCont = InCont
  { attrsI  :: Set Attr -- Required attributes
  , propsI  :: Set Prop -- Required properties
  , propsI' :: Set Prop -- Prohibited properties
  }
  deriving (Show)

inCont :: [NodeName] -> [NodeName] -> [NodeName] -> InCont
inCont as ps p's = InCont
  { attrsI  = Set.fromList $ map Attr as
  , propsI  = Set.fromList $ map Prop ps
  , propsI' = Set.fromList $ map Prop p's
  }

instance Conts.InCont InCont where
  type AT InCont = Attr
  type PT InCont = Prop

  emptyIn = InCont { attrsI = Set.empty
                   , propsI = Set.empty
                   , propsI' = Set.empty }

  match s c = attrsI c `Set.isSubsetOf` attrs s
           && propsI c `Set.isSubsetOf` props s
           && propsI' c `Set.disjoint`  props s

data OutAttrs =
    AddAttrs (Set Attr)
  | NewAttrs (Set Attr)
  deriving (Show)

data OutCont = OutCont
  { attrsO  :: OutAttrs -- Changes in attrs set
  , propsO  :: Set Prop -- Properties to be added
  , propsO' :: Set Prop -- Properties to be deleted
  }
  deriving (Show)

outCont' :: (Set Attr -> OutAttrs) -> [NodeName] -> [NodeName] -> [NodeName] -> OutCont
outCont' ctor as ps p's = OutCont
  { attrsO  = ctor $ Set.fromList $ map Attr as
  , propsO  = Set.fromList $ map Prop ps
  , propsO' = Set.fromList $ map Prop p's
  }

outCont, outContN :: [NodeName] -> [NodeName] -> [NodeName] -> OutCont
outCont  = outCont' AddAttrs
outContN = outCont' NewAttrs

instance Conts.OutCont OutCont where
  type AT' OutCont = Attr
  type PT' OutCont = Prop

  emptyOut = OutCont { attrsO = AddAttrs Set.empty
                     , propsO = Set.empty
                     , propsO' = Set.empty }

  update s c =
    State { attrs = attrs'
          , props = propsO c <> (props s \\ propsO' c) }
    where
      attrs' = case attrsO c of
        AddAttrs attrs' -> attrs' <> attrs s
        NewAttrs attrs' -> attrs'
