module Calco.DSL where

import           Calco.CoGraph
import           Calco.Conts
import           Calco.Defs
import           Calco.Graph

(->>) :: TermMarker -> Term -> (TermMarker, Term)
(->>) = (,)

infix 1 `ap0`
ap0 :: OutCont o => NodeName -> o -> (NodeName, Node i o)
ap0 nn o = (nn, Stream o)

infix 1 `ap1`
ap1 :: (InCont i, OutCont o) => NodeName -> (i, o) -> (NodeName, Node i o)
ap1 nn (i, o) = (nn, Tfm1 i o)

infix 1 `ap2`
ap2 :: (InCont i, OutCont o) => NodeName -> ((i, i), o) -> (NodeName, Node i o)
ap2 nn ((i1, i2), o) = (nn, Tfm2 i1 i2 o)

infix 3 <&>
(<&>) :: InCont i => i -> i -> (i, i)
(<&>) = (,)

infix 2 -->
(-->) :: OutCont o => a -> o -> (a, o)
(-->) = (,)
