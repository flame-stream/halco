{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

import           Control.Monad       (forM_)
import           Data.Either         (isRight)
import           Data.Map            (Map)
import           Debug.Trace
import           Test.Hspec

import           Halco.CGraph
import           Halco.Check
import           Halco.Conts
import           Halco.Defs
import qualified Halco.Examples.Trivial.Pets as Pets
import           Halco.Graph
import           Halco.GraphGen.Fast

main :: IO ()
main = hspec $ do
  testPets
  testFastGeneration

checkGraph' :: ContsContext s i o o1 o2
            => CGraph s i o o1 o2 -> Graph -> Bool
checkGraph' c = isRight . checkGraph c

testPets :: Spec
testPets = describe "Pets example checks" $ do
  forM_ (zip [1..] Pets.graphs) $ \(i :: Integer, graph) ->
    it ("Base on graph " <> show i) $ do
      graph `shouldSatisfy` checkGraph' Pets.cgraph

testFastGeneration :: Spec
testFastGeneration = describe "Fast generation test" $ do
  forM_ (zip [1..] $ genGraphs Pets.cgraph) $ \(i :: Integer, graph) ->
    it ("Generated graph " <> show i) $ do
      graph `shouldSatisfy` checkGraph' Pets.cgraph
