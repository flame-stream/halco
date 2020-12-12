module Calco.DSL where

import           Calco.Conts
import           Calco.Defs
import           Calco.Graph

(->>) :: TermMarker -> Term -> (TermMarker, Term)
(->>) = (,)

infix 1 `ap0`
ap0 :: OutCont o => TfmName -> o -> (TfmName, o)
ap0 = (,)

infix 1 `ap1`
ap1 :: (InCont i, OutCont o) => TfmName -> (i, o) -> (TfmName, (i, o))
ap1 = (,)

infix 1 `ap2`
ap2 :: (InCont i, OutCont o) => TfmName -> (i, i, o) -> (TfmName, (i, i, o))
ap2 = (,)

infix 3 <&>
(<&>) :: InCont i => i -> i -> (i, i)
(<&>) = (,)

infix 2 -->
(-->) :: (InCont i, OutCont o) => (i, i) -> o -> (i, i, o)
(i1, i2) --> o = (i1, i2, o)
