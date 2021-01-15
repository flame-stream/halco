{-# LANGUAGE GADTs #-}

module Calco.GraphGen.Utils where

import           Calco.CGraph (Env, stream, streams)
import           Calco.Conts
import           Calco.Graph  (Term (..), TermId)
import           Calco.State  (State)
import           Calco.Utils  ((<$$>))

type Source a p = (TermId, State a p)

graphSources :: ContContext a p i o
             => Env i o -> ([Source a p], [(TermId, Term)], TermId)
graphSources e =
  let enumeratedStreams = zip [1..] $ streams e
      sources = toState . stream e <$$> enumeratedStreams
      consts = Const <$$> enumeratedStreams
      tidMax = fst $ last enumeratedStreams
   in (sources, consts, tidMax)
