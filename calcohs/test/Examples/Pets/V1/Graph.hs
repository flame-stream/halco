module Examples.Pets.V1.Graph where

import           Calco.DSL
import           Calco.Graph

graph :: Graph
graph = Graph $ Map.fromList
  [ 1 ->> Const "pets"
  , 2 ->> Const "personsHavePets"
  , 3 ->> Const "persons"
  , 4 ->> App1 "personStats" 3
  , 5 ->> App2 "joinPetsHaves" 1 2
  , 6 ->> App1 "filterPersons" 4
  , 7 ->> App2 "joinPHPersons" 5 6
  , 8 ->> App1 "filterSameAge" 7
  , 9 ->> App1 "stats" 8
  , 10 ->> Const "breeds"
  , 11 ->> App2 "joinBreeds" 10 7
  , 12 ->> App1 "breedsStats" 11
  ]
