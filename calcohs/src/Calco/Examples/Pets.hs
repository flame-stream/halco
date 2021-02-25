{-# LANGUAGE TupleSections #-}

module Calco.Examples.Pets where

import           Data.Map         (Map)
import qualified Data.Map         as Map
import qualified Data.Set         as Set

import           Calco.CGraph     (CGraph, Semantics, env, semantics)
import           Calco.Conts.Base (InCont, OutCont, inCont, outCont, outContN)
import           Calco.DSL
import           Calco.Graph      (Graph, Node (..), graph)

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
  ]
  [ "petNamesStats" `ap1` inCont ["pet.name"]
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
  ]
  [ "joinPetsFriends" `ap2` inCont ["pet.id"] [] []
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
  [ 1 ->> Stream "pets"
  , 2 ->> Stream "persons"
  , 3 ->> Stream "friends"
  , 4 ->> Stream "species"

  , 5 ->> Tfm1 "petNamesStats" 1
  , 6 ->> Tfm1 "priceNames" 9
  , 7 ->> Tfm1 "nameSpeciesCorrelation" 12

  , 8 ->> Tfm1 "filterMesozoic" 5
  , 9 ->> Tfm1 "filterSameAge" 11

  , 10 ->> Tfm2 "joinPetsFriends" 8 3
  , 11 ->> Tfm2 "joinPersonsFriends" 2 10
  , 12 ->> Tfm2 "joinPetsSpecies" 11 4
  ]
