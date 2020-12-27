module Examples.Pets where

import           Test.Hspec

import           Calco.Check
import           Calco.CGraph
import           Calco.DSL
import           Calco.Graph

petsSemantics :: Semantics
petsSemantics = semantics
  [ -- Pets name statistics over all pets.
    "petNamesStats"
    -- If pet's master is of the same age as his pet than they get price.
    -- Pet should be less than thousand years old. No prize for dinosaurs and there's wisards.
  , "priceNames"
    -- How master's name correlate with the pet's beed. Also no dinosaurs should be considered.
  , "nameBreedCorr"
  ]

graphs :: [Graph]
graphs = [graph1]

graph1 :: Graph
graph1 = graph
  [ 1 ->> Const "pets"
  , 2 ->> Const "persons"
  , 3 ->> Const "friends"
  , 4 ->> Const "breeds"

  , 5 ->> App1 "petNamesStats" 1
  , 6 ->> App1 "priceNames" 9
  , 7 ->> App1 "nameBreedCorr" 12

  , 8 ->> App1 "filterDinosaurs" 5
  , 9 ->> App1 "filterSameAge" 11

  , 10 ->> App2 "joinPetsFriends" 8 3
  , 11 ->> App2 "joinPersonsFriends" 2 10
  , 12 ->> App2 "joinPetsBreeds" 11 4
  ]
