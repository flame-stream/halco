{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

import           Control.Monad            (forM_)
import           Data.Either              (isRight)
import           Data.Map                 (Map)
import           Debug.Trace
import           Test.Hspec

import           Calco.CGraph
import           Calco.Check
import           Calco.Conts
import           Calco.Defs
import qualified Calco.Examples.Pets      as Pets
import qualified Calco.Examples.Pets.Base as Base
import           Calco.Graph
import           Calco.GraphGen.Fast
import           Calco.Utils

main :: IO ()
main = hspec $ do
  testPets
  testFastGeneration

checkGraph' :: ContContext a p i o => CGraph i o -> Graph -> Bool
checkGraph' c = isRight . checkGraph c

testPets :: Spec
testPets = describe "Pets example checks" $ do
  forM_ (zip [1..] Pets.graphs) $ \(i :: Integer, graph) ->
    it ("Base on graph " <> show i) $ do
      graph `shouldSatisfy` checkGraph' Base.cgraph

testFastGeneration :: Spec
testFastGeneration = describe "Fast generation test" $ do
  forM_ (zip [1..] $ genGraphs Base.cgraph) $ \(i :: Integer, graph) ->
    it ("Generated graph " <> show i) $ do
      graph `shouldSatisfy` checkGraph' Base.cgraph
