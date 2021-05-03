{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}

module Halco.Conts.Trivial where

import           Data.Set          (Set, (\\))
import qualified Data.Set          as Set

import qualified Halco.Conts.Types as Types
import           Halco.Defs        (NodeName)
import qualified Halco.Defs        as Defs
import           Halco.State       (State (State), attrs, props)

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

instance Types.InCont InCont where
  type ATi InCont = Attr
  type PTi InCont = Prop

  emptyIn = InCont { attrsI  = Set.empty
                   , propsI  = Set.empty
                   , propsI' = Set.empty }

  match s c = attrsI c `Set.isSubsetOf` attrs s
           && propsI c `Set.isSubsetOf` props s
           && propsI' c `Set.disjoint`  props s

data OutAttrs =
    AddAttrs (Set Attr)
  | NewAttrs (Set Attr)
  deriving (Show)

updateAttrs :: State Attr p -> OutAttrs -> Set Attr
updateAttrs s = \case
  AddAttrs attrs' -> attrs' <> attrs s
  NewAttrs attrs' -> attrs'

data OutCont = OutCont
  { attrsO  :: OutAttrs -- Changes in attrs set
  , propsO  :: Set Prop -- Properties to be added
  , propsO' :: Set Prop -- Properties to be deleted
  }
  deriving (Show)

instance Types.OutCont OutCont where
  type ATo OutCont = Attr
  type PTo OutCont = Prop

  emptyOut = OutCont { attrsO  = AddAttrs Set.empty
                     , propsO  = Set.empty
                     , propsO' = Set.empty }

  update s c = State { attrs = updateAttrs s $ attrsO c
                     , props = propsO c <> (props s \\ propsO' c) }
