module Calco.StreamState where

import Control.Monad (foldM)
import Data.Function ((&))
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Set (Set, (\\))
import qualified Data.Set as Set

import Calco.Conts


-- Stream state after transformations
data State = State { attrs :: Set Attr
                   , props :: Set Prop }
  deriving (Show)

instance Semigroup State where
  (<>) = unionState

instance Monoid State where
  mempty = emptyState

emptyState :: State
emptyState = State { attrs = Set.empty
                   , props = Set.empty }

unionState :: State -> State -> State
unionState s1 s2 = State { attrs = (s1 & attrs) `Set.union` (s2 & attrs)
                         , props = (s1 & props) `Set.union` (s2 & props) }

updateState :: State -> OutCont -> State
updateState s c =
  let projected = case c & attrsO' of
                    DelAllAttrs -> Set.empty
                    DelAttrs attrs' -> (s & attrs) \\ attrs' in
  State { attrs = Set.union (c & attrsO) projected
        , props = Set.union (c & propsO) $ (s & props) \\ (c & propsO') }

matchState :: State -> InCont -> Bool
matchState s c = (c & attrsI) `Set.isSubsetOf` (s & attrs)
              && (c & propsI) `Set.isSubsetOf` (s & props)
              && (c & propsI') `Set.disjoint`  (s & props)
