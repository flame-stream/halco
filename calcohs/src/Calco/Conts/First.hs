{-# LANGUAGE TypeFamilies #-}

module Calco.Conts.First where

import qualified Calco.Conts as Conts
import           Calco.Defs  (StreamName)
import qualified Calco.Defs  as Defs
import           Calco.State (State (State), attrs, props)
import           Data.Set    (Set, (\\))
import qualified Data.Set    as Set

type AttrName = String
type PropName = String

data Attr = Attr StreamName AttrName
  deriving (Show, Ord, Eq)

instance Defs.Attr Attr

data Prop = Prop PropName | AttrProp Attr PropName
  deriving (Show, Ord, Eq)

instance Defs.Prop Prop

data InCont = InCont
  { attrsI  :: Set Attr -- Required attributes
  , propsI  :: Set Prop -- Required properties
  , propsI' :: Set Prop -- Prohibited properties
  }
  deriving (Show)

instance Conts.InCont InCont where
  type AT InCont = Attr
  type PT InCont = Prop

  emptyIn = InCont { attrsI = Set.empty
                   , propsI = Set.empty
                   , propsI' = Set.empty }

  match s c = attrsI c `Set.isSubsetOf` attrs s
           && propsI c `Set.isSubsetOf` props s
           && propsI' c `Set.disjoint`  props s

data DelAttrs = DelAllAttrs | DelAttrs (Set Attr)
  deriving (Show)

data OutCont = OutCont
  { attrsO  :: Set Attr -- Tfm adds attributes
  , attrsO' :: DelAttrs -- Attrs to be deleted
  , propsO  :: Set Prop -- Properties to be added
  , propsO' :: Set Prop -- Properties to be deleted
  }
  deriving (Show)

instance Conts.OutCont OutCont where
  type AT' OutCont = Attr
  type PT' OutCont = Prop

  emptyOut = OutCont { attrsO = Set.empty
                      , attrsO' = DelAttrs Set.empty
                      , propsO = Set.empty
                      , propsO' = Set.empty }

  update s c =
    let projected = case attrsO' c of
                      DelAllAttrs     -> Set.empty
                      DelAttrs attrs' -> attrs s \\ attrs'
     in State { attrs = Set.union (attrsO c) projected
              , props = Set.union (propsO c) $ props s \\ propsO' c }
