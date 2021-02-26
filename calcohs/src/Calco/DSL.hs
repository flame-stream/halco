module Calco.DSL where

import           Data.Map          (Map)
import qualified Data.Map          as Map

import           Calco.CGraph      (CStream (CStream), CTfm1 (CTfm1),
                                    CTfm2 (CTfm2))
import           Calco.Conts.Types (InCont, OutCont)
import           Calco.Defs        (NodeName)
import           Calco.Graph       (Node, NodeId)

(->>) :: a -> b -> (a, b)
(->>) = (,)

infix 1 `ap0`
ap0 :: OutCont o => NodeName -> o -> (NodeName, CStream o)
ap0 nn o = (nn, CStream o)

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

m :: Ord k => [(k, v)] -> Map k v
m = Map.fromList
