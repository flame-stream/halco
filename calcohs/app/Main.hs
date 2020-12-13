module Main where

import           Calco.Check
import           Calco.CoGraph
import           Calco.Conts
import           Calco.Conts.General as G
import           Calco.DSL
import           Calco.Defs
import           Calco.Graph
import qualified Data.Map          as Map
import qualified Data.Set          as Set

main :: IO ()
main = print $ checkGraph (env :: Env G.InCont G.OutCont) $ Graph Map.empty

env = emptyEnv { tfms2 = Map.fromList ["f" `ap2` fIn1 <&> fIn2 --> fOut] }
  where
    fIn1 = emptyIn
    fIn2 = emptyIn
    fOut = emptyOut
