{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

import           Control.Monad       (forM_)
import           Data.Either         (isRight)
import           Data.Map            (Map)
import           Debug.Trace
import           Test.Hspec

import           Calco.CGraph
import           Calco.Check
import           Calco.Conts
import qualified Calco.Conts.Base    as Base
import           Calco.Defs
import           Calco.Graph
import           Calco.GraphGen.Base
import           Calco.Utils

import qualified Examples.Pets       as Pets
import qualified Examples.Pets.Base  as Base

main :: IO ()
main = do
  putStrLn $ "Graphs:\n" <> show (genGraphs Base.cgraph)
  hspec $ do
    testPets

checkGraph' :: ContContext a p i o => CGraph i o -> Graph -> Bool
checkGraph' c = isRight . checkGraph c

testPets :: Spec
testPets = describe "Pets example checks" $ do
  forM_ (zip [1..] Pets.graphs) $ \(i :: Integer, graph) ->
    it ("Base on graph " <> show i) $ do
      graph `shouldSatisfy` checkGraph' Base.cgraph
