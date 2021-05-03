module Halco.DSL where

import           Data.Map     (Map)
import qualified Data.Map     as Map
import           Data.Set     (Set)
import qualified Data.Set     as Set

import           Halco.CGraph (CSource (CSource), CTfm1 (CTfm1), CTfm2 (CTfm2))
import           Halco.Conts  (InCont, OutCont)
import           Halco.Defs   (NodeName)
import           Halco.Graph  (Node, NodeId)

s :: Ord k => [k] -> Set k
s = Set.fromList

m :: Ord k => [(k, v)] -> Map k v
m = Map.fromList

(->>) :: a -> b -> (a, b)
(->>) = (,)

infix 1 `ap0`
ap0 :: OutCont s o => NodeName -> o -> (NodeName, CSource s o)
ap0 nn o = (nn, CSource o)

infix 1 `ap1`
ap1 :: (InCont s i, OutCont s o) => NodeName -> (i, o) -> (NodeName, CTfm1 s i o)
ap1 nn (i, o) = (nn, CTfm1 i o)

infix 1 `ap2`
ap2 :: (InCont s i, OutCont s o) => NodeName -> ((i, i), o) -> (NodeName, CTfm2 s i o)
ap2 nn ((i1, i2), o) = (nn, CTfm2 i1 i2 o)

infix 3 <&>
(<&>) :: InCont s i => i -> i -> (i, i)
(<&>) = (,)

infix 2 -->
(-->) :: OutCont s o => a -> o -> (a, o)
(-->) = (,)
