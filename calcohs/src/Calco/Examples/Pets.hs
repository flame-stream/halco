{-# LANGUAGE TupleSections #-}

module Calco.Examples.Pets where

import           Data.Map         (Map)
import qualified Data.Map         as Map
import qualified Data.Set         as Set

import           Calco.Beam
import           Calco.CGraph
import           Calco.Check
import           Calco.Conts.Base
import           Calco.DSL
import           Calco.Graph

petsSemantics :: Semantics
petsSemantics = semantics
  [ -- Pets name statistics over all pets.
    "petNamesStats"
    -- If pet's master is of the same age as his pet than they get price.
    -- Pet should be not from mesozoic era. No prize for dinosaurs and there's wisards.
  , "priceNames"
    -- How master's name correlate with the pet's species.
    -- Also no pets from mesozoic era should be considered.
  , "nameSpeciesCorrelation"
  ]

cgraph :: CGraph InCont OutCont
cgraph = (, petsSemantics) $ env
  [ "pets"    `ap0` outCont ["pet.id", "pet.name", "pet.age", "pet.speciesId"] [] []
  , "persons" `ap0` outCont ["person.id", "person.name", "person.age"]         [] []
  , "friends" `ap0` outCont ["friend.personId", "friend.petId"]                [] []
  , "species" `ap0` outCont ["species.id", "species.name"]                     [] []

  , "petNamesStats" `ap1` inCont ["pet.name"]
                                 []
                                 ["sameAge", "noFromMesozoic"] -- To consider all pets
                     -->  outCont [] [] []

  , "priceNames" `ap1` inCont ["person.name", "pet.name"] ["sameAge", "noFromMesozoic"] []
                  -->  outContN [] [] []

  , "nameSpeciesCorrelation" `ap1` inCont ["person.name", "species.name"] ["noFromMesozoic"] []
                              -->  outContN [] [] []

  , "filterMesozoic" `ap1` inCont ["pet.age"] [] []
                      -->  outCont [] ["noFromMesozoic"] []

  , "filterSameAge" `ap1` inCont ["pet.age", "person.age"] [] []
                     -->  outCont [] ["sameAge"] []

  , "joinPetsFriends" `ap2` inCont ["pet.id"] [] []
                       <&>  inCont ["friend.petId"] [] []
                       -->  outCont [] [] []

  , "joinPersonsFriends" `ap2` inCont ["person.id"] [] []
                          <&>  inCont ["friend.personId"] [] []
                          -->  outCont [] [] []

  , "joinPetsSpecies" `ap2` inCont ["pet.speciesId"] [] []
                       <&>  inCont ["species.id"] [] []
                       -->  outCont [] [] []
  ]

graphs :: [Graph]
graphs = [graph1]

-- https://dreampuf.github.io/GraphvizOnline/#digraph%20%22graph1%22%20%7B%0A%0A%22joinPetsSpecies%22%0A%22joinPersonsFriends%22%0A%22joinPetsFriends%22%0A%22filterSameAge%22%0A%22filterMesozoic%22%0A%22nameSpeciesCorrelation%22%0A%22priceNames%22%0A%22petNamesStats%22%0A%22species%22%0A%22friends%22%0A%22persons%22%0A%22pets%22%0A%22joinPersonsFriends%22%20-%3E%20%22joinPetsSpecies%22%0A%22species%22%20-%3E%20%22joinPetsSpecies%22%0A%22persons%22%20-%3E%20%22joinPersonsFriends%22%0A%22joinPetsFriends%22%20-%3E%20%22joinPersonsFriends%22%0A%22filterMesozoic%22%20-%3E%20%22joinPetsFriends%22%0A%22friends%22%20-%3E%20%22joinPetsFriends%22%0A%22joinPersonsFriends%22%20-%3E%20%22filterSameAge%22%0A%22petNamesStats%22%20-%3E%20%22filterMesozoic%22%0A%22joinPetsSpecies%22%20-%3E%20%22nameSpeciesCorrelation%22%0A%22filterSameAge%22%20-%3E%20%22priceNames%22%0A%22pets%22%20-%3E%20%22petNamesStats%22%0A%7D
graph1 :: Graph
graph1 = graph
  [ 1 ->> Const "pets"
  , 2 ->> Const "persons"
  , 3 ->> Const "friends"
  , 4 ->> Const "species"

  , 5 ->> App1 "petNamesStats" 1
  , 6 ->> App1 "priceNames" 9
  , 7 ->> App1 "nameSpeciesCorrelation" 12

  , 8 ->> App1 "filterMesozoic" 5
  , 9 ->> App1 "filterSameAge" 11

  , 10 ->> App2 "joinPetsFriends" 8 3
  , 11 ->> App2 "joinPersonsFriends" 2 10
  , 12 ->> App2 "joinPetsSpecies" 11 4
  ]

type E = Map String String

graph1Beam :: Stream E -> Stream E -> Stream E -> Stream E
           -> ( Stream E -- Pet names
              , Stream E -- Perople witn pets
              , Stream E -- speciess
              )
graph1Beam pets persons friends species =
  let petNamesStats = undefined
      priceNames = undefined
      nameSpeciesCorrelation = undefined
   in (petNamesStats, priceNames, nameSpeciesCorrelation)
