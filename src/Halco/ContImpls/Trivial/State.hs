{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Halco.ContImpls.Trivial.State where

import           Data.Set                     (Set)
import qualified Data.Set                     as Set

import           Halco.ContImpls.Trivial.Defs (Attr, Prop)
import qualified Halco.State                  as S
import           Halco.Utils.Classes          (Empty (..))

-- Sorts of information about the data:
-- attributes that are available on each dataflow element;
-- properties that are satisfied for the attributes.
data State = State
  { attrs :: Set Attr
  , props :: Set Prop }
  deriving (Show, Eq, Ord)

instance Empty State where
  empty = State { attrs = Set.empty
                , props = Set.empty }

instance Semigroup State where
  (<>) = union

instance Monoid State where
  mempty = empty

deriving instance S.State State

union :: State -> State -> State
union s1 s2 = State { attrs = attrs s1 <> attrs s2
                    , props = props s1 <> props s2 }
