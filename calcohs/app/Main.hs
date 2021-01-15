module Main where

import           Calco.Examples.Pets.Base
import           Calco.GraphGen.Fast

main :: IO ()
main = putStrLn $ "Graphs:\n" <> show (genGraphs cgraph)
