module Halco.ContImpls.Pragmatic.Scheme where

import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Set                       (Set)
import qualified Data.Set                       as Set

import           Halco.ContImpls.Pragmatic.Defs (Attr, OpaqueProp)
import qualified Halco.Scheme                   as S
import           Halco.Utils.Classes            (Empty (..))

data Scheme =
    Scheme
    { attrs :: Set Attr       -- Attributes of the stream elements
    , mods  :: Set OpaqueProp -- Modifications applied to the stream
    , props :: Set OpaqueProp --
    }
  | BatchScheme
    {}
  deriving (Show, Eq, Ord)
