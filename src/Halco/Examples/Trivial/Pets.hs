{-# LANGUAGE TupleSections #-}

module Halco.Examples.Trivial.Pets where

import           Data.Map            (Map, (!))
import qualified Data.Map            as Map
import qualified Data.Set            as Set

import           Halco.Beam          (coReduceNode', pardoNodeP, reduceNode)
import           Halco.CGraph        (CGraph, Env (Env), Semantics)
import qualified Halco.CGraph        as CGraph
import           Halco.Conts
import           Halco.DSL
import           Halco.EGraph
import           Halco.Graph         (Graph, Node (..), graph)
import           Halco.Trivial.Conts
import qualified Halco.Trivial.Conts as Trivial
import           Halco.Trivial.DSL
import           Halco.Trivial.State (State)
import           Halco.Utils.Classes (Empty (..))

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

cgraph :: CGraph State Trivial.InCont Trivial.OutCont
cgraph = (, semantics) $ Env
  { CGraph.sources = m
    [ "pets"    `ap0` empty
      { attrsO = NewAttrs $ attrs' "pet" ["id", "name", "age", "speciesId"] }
    , "persons" `ap0` empty
      { attrsO = NewAttrs $ attrs' "person" ["id", "name", "age"] }
    , "friends" `ap0` empty
      { attrsO = NewAttrs $ attrs' "friend" ["personId", "petId"] }
    , "species" `ap0` empty
      { attrsO = NewAttrs $ attrs' "species" ["id", "name"] }
    ]
  , CGraph.tfms1 = m
    [ "petNamesStats"
      `ap1` empty { attrsI = attr "pet.name"
                  , propsI' = props ["sameAge", "noFromMesozoic"] } -- To consider all pets
       -->  empty

    , "priceNames"
      `ap1` empty { attrsI = attrs ["person.name", "pet.name"]
                  , propsI = props ["sameAge", "noFromMesozoic"] }
       -->  empty { attrsO = delAttrs }

    , "nameSpeciesCorrelation"
      `ap1` empty { attrsI = attrs ["person.name", "species.name"]
                  , propsI = prop "noFromMesozoic" }
       -->  empty { attrsO = delAttrs }

    , "filterMesozoic"
      `ap1` empty { attrsI = attr "pet.age" }
       -->  empty { propsO = prop "noFromMesozoic" }

    , "filterSameAge"
      `ap1` empty { attrsI = attrs ["pet.age", "person.age"] }
       -->  empty { propsO = prop "sameAge" }
    ]
  , CGraph.tfms2 = m
    [ "joinPetsFriends"
      `ap2` empty { attrsI = attr "pet.id" }
       <&>  empty { attrsI = attr "friend.petId" }
       -->  empty

    , "joinPersonsFriends"
      `ap2` empty { attrsI = attr "person.id" }
       <&>  empty { attrsI = attr "friend.personId" }
       -->  empty

    , "joinPetsSpecies"
      `ap2` empty { attrsI = attr "pet.speciesId" }
       <&>  empty { attrsI = attr "species.id" }
       -->  empty
    ]
  }

graphs :: [Graph]
graphs = [graph1]

-- https://dreampuf.github.io/GraphvizOnline/#digraph%20%22graph1%22%20%7B%0A%0A%22joinPetsSpecies%22%0A%22joinPersonsFriends%22%0A%22joinPetsFriends%22%0A%22filterSameAge%22%0A%22filterMesozoic%22%0A%22nameSpeciesCorrelation%22%0A%22priceNames%22%0A%22petNamesStats%22%0A%22species%22%0A%22friends%22%0A%22persons%22%0A%22pets%22%0A%22joinPersonsFriends%22%20-%3E%20%22joinPetsSpecies%22%0A%22species%22%20-%3E%20%22joinPetsSpecies%22%0A%22persons%22%20-%3E%20%22joinPersonsFriends%22%0A%22joinPetsFriends%22%20-%3E%20%22joinPersonsFriends%22%0A%22filterMesozoic%22%20-%3E%20%22joinPetsFriends%22%0A%22friends%22%20-%3E%20%22joinPetsFriends%22%0A%22joinPersonsFriends%22%20-%3E%20%22filterSameAge%22%0A%22petNamesStats%22%20-%3E%20%22filterMesozoic%22%0A%22joinPetsSpecies%22%20-%3E%20%22nameSpeciesCorrelation%22%0A%22filterSameAge%22%20-%3E%20%22priceNames%22%0A%22pets%22%20-%3E%20%22petNamesStats%22%0A%7D
graph1 :: Graph
graph1 = graph
  [ 1 ->> Source "pets"
  , 2 ->> Source "persons"
  , 3 ->> Source "friends"
  , 4 ->> Source "species"

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
  { sources = m
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

pets :: ESource E
pets =
  [ pet ["1", "a", "12", "3"]
  , pet ["2", "b", "1", "1"]
  , pet ["3", "bobik", "10005000", "2"]
  , pet ["4", "d", "10", "1"]
  ]
  where
    pet = m . zip ["pet.id", "pet.name", "pet.age", "pet.speciesId"]

persons :: ESource E
persons =
  [ person ["1", "Bob", "5"]
  , person ["2", "Sarah", "1"]
  , person ["3", "Lee Jae Dong", "30"]
  , person ["4", "Stork", "10005000"]
  , person ["5", "Byun", "28"]
  ]
  where
    person = m . zip ["person.id", "person.name", "person.age"]

friends :: ESource E
friends =
  [ friend ["1", "2"]
  , friend ["4", "3"]
  ]
  where
    friend = m . zip ["friend.personId", "friend.petId"]

species :: ESource E
species =
  [ specie ["3", "dog"]
  , specie ["1", "cat"]
  , specie ["2", "Tyrannosaurus"]
  ]
  where
    specie = m . zip ["species.id", "species.name"]
