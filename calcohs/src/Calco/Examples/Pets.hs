{-# LANGUAGE TupleSections #-}

module Calco.Examples.Pets where

import           Data.Map          (Map, (!))
import qualified Data.Map          as Map
import qualified Data.Set          as Set

import           Calco.Beam        (coReduceNode', pardoNodeP, reduceNode)
import           Calco.CGraph      (CGraph, Env (Env), Semantics)
import qualified Calco.CGraph      as CGraph
import           Calco.Conts.Impl
import qualified Calco.Conts.Impl  as Impl
import           Calco.Conts.Types
import           Calco.DSL
import           Calco.Eval
import           Calco.Graph       (Graph, Node (..), empty, graph)

semantics :: Semantics
semantics = s
  [ -- Pets name statistics over all pets.
    "petNamesStats"
    -- If pet's master is of the same age as his pet than they get price.
    -- Pet should be not from mesozoic era. No prize for dinosaurs and there's wisards.
  , "priceNames"
    -- How master's name correlate with the pet's species.
    -- Also no pets from mesozoic era should be considered.
  , "nameSpeciesCorrelation"
  ]

cgraph :: CGraph Impl.InCont Impl.OutCont
cgraph = (, semantics) $ Env
  { CGraph.streams = m
    [ "pets"    `ap0` emptyOut { attrsO = NewAttrs $ attrs' "pet" ["id", "name", "age", "speciesId"] }
    , "persons" `ap0` emptyOut { attrsO = NewAttrs $ attrs' "person" ["id", "name", "age"] }
    , "friends" `ap0` emptyOut { attrsO = NewAttrs $ attrs' "friend" ["personId", "petId"] }
    , "species" `ap0` emptyOut { attrsO = NewAttrs $ attrs' "species" ["id", "name"] }
    ]
  , CGraph.tfms1 = m
    [ "petNamesStats" `ap1` emptyIn { attrsI = attr "pet.name"
                                    , propsI' = props ["sameAge", "noFromMesozoic"] } -- To consider all pets
                       -->  emptyOut

    , "priceNames" `ap1` emptyIn { attrsI = attrs ["person.name", "pet.name"]
                                 , propsI = props ["sameAge", "noFromMesozoic"] }
                    -->  emptyOut { attrsO = delAttrs }

    , "nameSpeciesCorrelation" `ap1` emptyIn { attrsI = attrs ["person.name", "species.name"]
                                             , propsI = prop "noFromMesozoic" }
                                -->  emptyOut { attrsO = delAttrs }

    , "filterMesozoic" `ap1` emptyIn { attrsI = attr "pet.age" }
                        -->  emptyOut { propsO = prop "noFromMesozoic" }

    , "filterSameAge" `ap1` emptyIn { attrsI = attrs ["pet.age", "person.age"] }
                       -->  emptyOut { propsO = prop "sameAge" }
    ]
  , CGraph.tfms2 = m
    [ "joinPetsFriends" `ap2` emptyIn { attrsI = attr "pet.id" }
                         <&>  emptyIn { attrsI = attr "friend.petId" }
                         -->  emptyOut

    , "joinPersonsFriends" `ap2` emptyIn { attrsI = attr "person.id" }
                            <&>  emptyIn { attrsI = attr "friend.personId" }
                            -->  emptyOut

    , "joinPetsSpecies" `ap2` emptyIn { attrsI = attr "pet.speciesId" }
                         <&>  emptyIn { attrsI = attr "species.id" }
                         -->  emptyOut
    ]
  }

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

type E = Map String String

env' :: Env' E
env' = Env'
  { streams = m
    [ "pets" ->> pets
    , "persons" ->> persons
    , "friends" ->> friends
    , "species" ->> species
    ]
  , tfms1 = m
    [ "petNamesStats" ->> id
    , "priceNames" ->> id
    , "nameSpeciesCorrelation" ->> id
    , "filterMesozoic" ->> pardoNodeP ((< (100500 :: Integer)) . read . (! "pet.age"))
    , "filterSameAge" ->> pardoNodeP (\e -> e ! "person.age" == e ! "pet.age")
    ]
  , tfms2 = m
    [ "joinPetsFriends" ->> coReduceNode' (! "pet.id") (! "friend.petId")
    , "joinPersonsFriends" ->> coReduceNode' (! "person.id") (! "friend.personId")
    , "joinPetsSpecies" ->> coReduceNode' (! "pet.speciesId") (! "species.id")
    ]
  }

pets :: EStream E
pets =
  [ pet ["1", "a", "12", "3"]
  , pet ["2", "b", "1", "1"]
  , pet ["3", "bobik", "10005000", "2"]
  , pet ["4", "d", "10", "1"]
  ]
  where
    pet = m . zip ["pet.id", "pet.name", "pet.age", "pet.speciesId"]

persons :: EStream E
persons =
  [ person ["1", "Bob", "5"]
  , person ["2", "Sarah", "1"]
  , person ["3", "Lee Jae Dong", "30"]
  , person ["4", "Stork", "10005000"]
  , person ["5", "Byun", "28"]
  ]
  where
    person = m . zip ["person.id", "person.name", "person.age"]

friends :: EStream E
friends =
  [ friend ["1", "2"]
  , friend ["4", "3"]
  ]
  where
    friend = m . zip ["friend.personId", "friend.petId"]

species :: EStream E
species =
  [ specie ["3", "dog"]
  , specie ["1", "cat"]
  , specie ["2", "Tyrannosaurus"]
  ]
  where
    specie = m . zip ["species.id", "species.name"]
