module Halco.ContImpls.Pragmatic.State where

import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Set                       (Set)
import qualified Data.Set                       as Set

import           Halco.ContImpls.Pragmatic.Defs (Attr, OpaqueProp)
import qualified Halco.State                    as S
import           Halco.Utils.Classes            (Empty (..))

data State =
    State
    { attrs :: Set Attr       -- Attributes of the stream elements
    , mods  :: Set OpaqueProp -- Modifications applied to the stream
    , props :: Set OpaqueProp --
    }
  | BatchState
    {}
  deriving (Show, Eq, Ord)
