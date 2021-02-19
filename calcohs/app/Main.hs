{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad       (forM_)

import           Calco.Examples.Pets
import           Calco.Graph         (graph2Dot)
import           Calco.GraphGen.Fast

main :: IO ()
main = forM_ (zip [1..] $ genGraphs cgraph) $ \(i :: Integer, g) ->
  putStrLn $ graph2Dot g (show i)
