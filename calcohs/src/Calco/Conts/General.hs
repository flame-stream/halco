{-# LANGUAGE TypeFamilies #-}

module Calco.Conts.General where

import qualified Calco.Conts as Conts
import qualified Calco.Defs  as Defs
import           Calco.State (State (State), attrs, props)
import           Data.Set    (Set, (\\))
import qualified Data.Set    as Set

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
  , propsO  :: Set Prop -- Properties to be added
  , propsO' :: Set Prop -- Properties to be deleted
  }
  deriving (Show)

instance Conts.OutCont OutCont where
  type AT' OutCont = Attr
  type PT' OutCont = Prop

  emptyOut = OutCont { attrsO = Set.empty
                     , propsO = Set.empty
                     , propsO' = Set.empty }

  update s c =
    State { attrs = attrsO c <> attrs s
          , props = propsO c <> (props s \\ propsO' c) }
