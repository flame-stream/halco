{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad               (forM_)
import           Data.Map                    ((!))

import           Halco.EGraph
import           Halco.Examples.Trivial.Pets
import           Halco.Graph                 (graph2Dot)
import           Halco.GraphGen.Fast

main :: IO ()
main = do
  let graph = head $ genGraphs cgraph
  let res = eval graph env'
  print graph
  putStrLn ""
  putStrLn $ "Result: " <> show res
  -- putStrLn ""
  -- putStrLn $ "petNamesStats: " <> show (res ! "petNamesStats")
  -- putStrLn ""
  -- putStrLn $ "priceNames: " <> show (res ! "priceNames")
  -- putStrLn ""
  -- putStrLn $ "nameSpeciesCorrelation: " <> show (res ! "nameSpeciesCorrelation")
  -- putStrLn ""

  -- undefined

  forM_ (zip [1..] $ genGraphs cgraph) $ \(i :: Integer, g) ->
    putStrLn $ graph2Dot g (show i)
