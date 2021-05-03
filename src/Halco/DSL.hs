module Halco.DSL where

import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set

import           Halco.CGraph        (CSource (CSource), CTfm1 (CTfm1),
                                      CTfm2 (CTfm2))
import           Halco.Conts.Trivial (Attr (..), OutAttrs (NewAttrs), Prop (..))
import           Halco.Conts.Types   (InCont, OutCont)
import           Halco.Defs          (NodeName)
import           Halco.Graph         (Node, NodeId)

attr :: NodeName -> Set Attr
attr = Set.singleton . Attr

attrs :: [NodeName] -> Set Attr
attrs =  Set.fromList . map Attr

attrs' :: NodeName -> [NodeName] -> Set Attr
attrs' prefix = Set.fromList . map (Attr . (prefix ++) . ('.' :))

prop :: NodeName -> Set Prop
prop = Set.singleton . Prop

props :: [NodeName] -> Set Prop
props = Set.fromList . map Prop

(->>) :: a -> b -> (a, b)
(->>) = (,)

infix 1 `ap0`
ap0 :: OutCont o => NodeName -> o -> (NodeName, CSource o)
ap0 nn o = (nn, CSource o)

infix 1 `ap1`
ap1 :: (InCont i, OutCont o) => NodeName -> (i, o) -> (NodeName, CTfm1 i o)
ap1 nn (i, o) = (nn, CTfm1 i o)

infix 1 `ap2`
ap2 :: (InCont i, OutCont o) => NodeName -> ((i, i), o) -> (NodeName, CTfm2 i o)
ap2 nn ((i1, i2), o) = (nn, CTfm2 i1 i2 o)

infix 3 <&>
(<&>) :: InCont i => i -> i -> (i, i)
(<&>) = (,)

infix 2 -->
(-->) :: OutCont o => a -> o -> (a, o)
(-->) = (,)

delAttrs :: OutAttrs
delAttrs = NewAttrs Set.empty

s :: Ord k => [k] -> Set k
s = Set.fromList

m :: Ord k => [(k, v)] -> Map k v
m = Map.fromList
