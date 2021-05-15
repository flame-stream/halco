{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Halco.ContImpls.Trivial.Scheme where

import           Data.Set                     (Set)
import qualified Data.Set                     as Set

import           Halco.ContImpls.Trivial.Defs (Attr, Prop)
import qualified Halco.Scheme                 as S
import           Halco.Utils.Classes          (Empty (..))

-- Sorts of information about the data:
-- attributes that are available on each dataflow element;
-- properties that are satisfied for the attributes.
data Scheme = Scheme
  { attrs :: Set Attr
  , props :: Set Prop }
  deriving (Show, Eq, Ord)

instance Empty Scheme where
  empty = Scheme { attrs = Set.empty
                , props = Set.empty }

instance Semigroup Scheme where
  (<>) = union

instance Monoid Scheme where
  mempty = empty

deriving instance S.Scheme Scheme

union :: Scheme -> Scheme -> Scheme
union s1 s2 = Scheme { attrs = attrs s1 <> attrs s2
                    , props = props s1 <> props s2 }
