{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Halco.Trivial.Conts where

import           Data.Set            (Set, (\\))
import qualified Data.Set            as Set

import qualified Halco.Conts         as Conts
import           Halco.Trivial.Defs  (Attr, Prop)
import           Halco.Trivial.State (State (..))
import           Halco.Utils.Classes (Empty (..))

data InCont = InCont
  { attrsI  :: Set Attr -- Required attributes
  , propsI  :: Set Prop -- Required properties
  , propsI' :: Set Prop -- Prohibited properties
  }
  deriving (Show)

instance Empty InCont where
  empty = InCont { attrsI  = Set.empty
                 , propsI  = Set.empty
                 , propsI' = Set.empty }

instance Conts.InCont State InCont where
  match s i = attrsI i `Set.isSubsetOf` attrs s
           && propsI i `Set.isSubsetOf` props s
           && propsI' i `Set.disjoint`  props s

data OutAttrs =
    AddAttrs (Set Attr)
  | NewAttrs (Set Attr)
  deriving (Show)

updateAttrs :: State -> OutAttrs -> Set Attr
updateAttrs s = \case
  AddAttrs attrs' -> attrs' <> attrs s
  NewAttrs attrs' -> attrs'

data OutCont = OutCont
  { attrsO  :: OutAttrs -- Changes in attrs set
  , propsO  :: Set Prop -- Properties to be added
  , propsO' :: Set Prop -- Properties to be deleted
  }
  deriving (Show)

instance Empty OutCont where
  empty = OutCont { attrsO  = AddAttrs Set.empty
                  , propsO  = Set.empty
                  , propsO' = Set.empty }

instance Conts.OutCont State OutCont where
  update s o = State { attrs = updateAttrs s $ attrsO o
                     , props = propsO o <> (props s \\ propsO' o) }
