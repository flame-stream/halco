module Halco.DSL where

import           Data.Map     (Map)
import qualified Data.Map     as Map
import           Data.Set     (Set)
import qualified Data.Set     as Set

import           Halco.CGraph (COp1 (COp1), COp2 (COp2), CSource (CSource))
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
ap1 :: (InCont s i, OutCont s o) => NodeName -> (i, o) -> (NodeName, COp1 s i o)
ap1 nn (i, o) = (nn, COp1 i o)

infix 1 `ap2`
ap2 :: (InCont s i, OutCont s o) => NodeName -> ((i, i), o) -> (NodeName, COp2 s i o)
ap2 nn ((i1, i2), o) = (nn, COp2 i1 i2 o)

infix 3 <&>
(<&>) :: InCont s i => i -> i -> (i, i)
(<&>) = (,)

infix 2 -->
(-->) :: OutCont s o => a -> o -> (a, o)
(-->) = (,)
