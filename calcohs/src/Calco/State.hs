{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Calco.State where

import           Calco.Defs
import           Data.Set   (Set)
import qualified Data.Set   as Set

type StateContext a p = (Attr a, Prop p)

data State a p where
  State :: StateContext a p
        => { attrs :: Set a
           , props :: Set p
           }                -> State a p

deriving instance Show (State a p)

instance StateContext a p => Semigroup (State a p) where
  (<>) = union

instance StateContext a p => Monoid (State a p) where
  mempty = empty

empty :: StateContext a p => State a p
empty = State { attrs = Set.empty
              , props = Set.empty }

union :: StateContext a p => State a p -> State a p -> State a p
union s1 s2 = State { attrs = attrs s1 `Set.union` attrs s2
                    , props = props s1 `Set.union` props s2 }
